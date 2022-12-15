/*
 * This file is part of mpv.
 *
 * mpv is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * mpv is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with mpv.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "ao.h"
#include "internal.h"
#include "audio/format.h"
#include "osdep/timer.h"
#include "options/m_option.h"
#include "common/msg.h"
#include "ao_coreaudio_utils.h"
#include "ao_coreaudio_chmap.h"

#include <libavcodec/packet.h>
#include <libavformat/avformat.h>

#import <CoreAudio/CoreAudioTypes.h>
#import <AudioToolbox/AudioToolbox.h>
#import <AVFoundation/AVFoundation.h>
#import <mach/mach_time.h>

#if TARGET_OS_IPHONE
#define HAVE_AVAUDIOSESSION
#endif

struct API_AVAILABLE(macos(10.13), ios(11.0)) priv {
    CMFormatDescriptionRef desc;
    AVSampleBufferAudioRenderer *renderer;
    AVSampleBufferRenderSynchronizer *synchronizer;
    AVFormatContext *avf;
    AVPacket *pkt;
    AudioStreamBasicDescription asbd;
    double enqueued;
    int spatialize;
};

#define PRIV_SIZE (sizeof(void*) * 5 + sizeof(AudioStreamBasicDescription) + sizeof(double) + sizeof(long))


static bool enqueue_buf(struct ao *ao, void *buf, int bufsize,
                        uint64_t samples, int sample_rate, bool spdif) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct priv *p = ao->priv;

    CMSampleBufferRef sBuf = NULL;
    CMBlockBufferRef bBuf = NULL;
    bool ret = false;

    OSStatus err;
    err = CMBlockBufferCreateWithMemoryBlock(NULL, NULL, bufsize, NULL, NULL, 0, bufsize, 0, &bBuf);
    CHECK_CA_ERROR("failed to create CMBlockBuffer");

    err = CMBlockBufferReplaceDataBytes(buf, bBuf, 0, bufsize);
    CHECK_CA_ERROR("failed to append data to CMBlockBuffer");

    AudioStreamPacketDescription packet = {
        .mDataByteSize = bufsize,
        .mStartOffset = 0,
        .mVariableFramesInPacket = samples,
    };

    err = CMAudioSampleBufferCreateReadyWithPacketDescriptions(NULL,    // allocator
                               bBuf,    // dataBuffer
                               p->desc, // formatDescription
                               spdif ? 1 : samples,  // numSamples
                               CMTimeMakeWithSeconds(p->enqueued, 1000000), // presentationTimeStamp
                               spdif ? &packet : NULL, // packetDescriptions
                               &sBuf);
    CHECK_CA_ERROR("failed to create CMSampleBuffer");

    [p->renderer enqueueSampleBuffer:sBuf];
    p->enqueued += (double)samples / sample_rate;

    ret = true;

coreaudio_error:
    if (sBuf)
        CFRelease(sBuf);
    if (bBuf)
        CFRelease(bBuf);

    return ret;
}

static OSStatus read_cb(void *inClientData, SInt64 inPosition, UInt32 requestCount,
                        void *buffer, UInt32 *actualCount) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct ao *ao = inClientData;
    struct priv *p = ao->priv;

    size_t size = p->pkt->size;
    if (inPosition >= size) {
        *actualCount = 0;
        return 0;
    }

    size -= inPosition;
    if (requestCount < size)
        size = requestCount;
    *actualCount = size;

    if (size > 0)
        memcpy(buffer, p->pkt->data + inPosition, size);

    return 0;
}

static SInt64 get_size_cb(void *inClientData) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct ao *ao = inClientData;
    struct priv *p = ao->priv;

    return p->pkt->size;
}


static bool enqueue_data(struct ao *ao, void *buf, int samples) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct priv *p = ao->priv;
    int bufsize = samples * ao->sstride;
    OSStatus err;
    bool success = true;

    if (af_fmt_is_spdif(ao->format)) {
        int ret;
        AVIOContext *s = p->avf->pb;
        AudioFileTypeID file_type = kAudioFileAC3Type;
        s->buf_ptr = s->buffer = buf;
        s->buffer_size = bufsize;
        s->buf_end = s->buffer + bufsize;
        s->pos = 0;
        s->eof_reached = 0;

        if (ao->format == AF_FORMAT_S_AAC)
            file_type = kAudioFileAAC_ADTSType;
        else if (ao->format == AF_FORMAT_S_MP3)
            file_type = kAudioFileMP3Type;

        while ((ret = av_read_frame(p->avf, p->pkt)) >= 0) {
            AudioFileID file = NULL;
            err = AudioFileOpenWithCallbacks(ao, read_cb, NULL, get_size_cb, NULL, file_type, &file);
            CHECK_CA_ERROR("unable to parse packet properties");

            AudioStreamBasicDescription asbd;
            UInt32 io_size = sizeof(asbd);
            err = AudioFileGetProperty(file, kAudioFilePropertyDataFormat, &io_size, &asbd);
            AudioFileClose(file);
            CHECK_CA_ERROR("unable to get ASBD for packet");

            if (p->asbd.mSampleRate != asbd.mSampleRate ||
                p->asbd.mChannelsPerFrame != asbd.mChannelsPerFrame ||
                p->asbd.mFormatID != asbd.mFormatID ||
                p->asbd.mFramesPerPacket != asbd.mFramesPerPacket) {
                if (p->desc) {
                    CFRelease(p->desc);
                    p->desc = NULL;
                }

                memcpy(&p->asbd, &asbd, sizeof(asbd));

                err = CMAudioFormatDescriptionCreate(NULL,
                                                     &p->asbd,
                                                     0, NULL,
                                                     0, NULL,
                                                     NULL,
                                                     &p->desc);
                CHECK_CA_ERROR("unable to create format description");
            }

            if (!enqueue_buf(ao, p->pkt->data, p->pkt->size, asbd.mFramesPerPacket, asbd.mSampleRate, true)) {
                success = false;
                goto coreaudio_error;
            }

            av_packet_unref(p->pkt);
        }


coreaudio_error:
        av_packet_unref(p->pkt);
        s->buffer = NULL;
    } else {
        success = enqueue_buf(ao, buf, bufsize, samples, ao->samplerate, false);
    }

    // If we still want more input, wake ourselves up again
    if (success && [p->renderer isReadyForMoreMediaData])
        ao_wakeup_playthread(ao);

    return success;
}

static bool avf_write(struct ao *ao, void **data, int samples)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        return enqueue_data(ao, *data, samples);
    } else {
        return false;
    }
}

static void set_media_role(enum aocontrol_media_role role)
{
#ifdef HAVE_AVAUDIOSESSION
    AVAudioSession *instance = AVAudioSession.sharedInstance;
    [instance setMode:((role == AOCONTROL_MEDIA_ROLE_MUSIC) ? AVAudioSessionModeMoviePlayback : AVAudioSessionModeDefault) error:nil];
#endif
}

static bool init_renderer(struct ao *ao) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct priv *p = ao->priv;
    OSStatus err;
    AudioChannelLayout *layout = NULL;
    bool ret = false;

#if !TARGET_OS_IPHONE
    if (ao->device && ao->device[0])
        p->renderer.audioOutputDeviceUniqueID = (__bridge NSString*)cfstr_from_cstr(ao->device);
#endif

#ifdef HAVE_AVAUDIOSESSION
    AVAudioSession *instance = AVAudioSession.sharedInstance;
    NSInteger maxChannels = instance.maximumOutputNumberOfChannels;
    NSInteger prefChannels = MIN(maxChannels, ao->channels.num);

    [instance setCategory:AVAudioSessionCategoryPlayback error:nil];
    [instance setMode:AVAudioSessionModeMoviePlayback error:nil];
    [instance setActive:YES error:nil];
    [instance setPreferredOutputNumberOfChannels:prefChannels error:nil];
#endif

    // todo: add support for planar formats
    ao->format = af_fmt_from_planar(ao->format);
    ca_fill_asbd(ao, &p->asbd);

    size_t layout_size;
    layout = ca_layout_get(ao, &layout_size, &ao->channels);
    if (!layout)
        layout_size = 0;

    err = CMAudioFormatDescriptionCreate(NULL,
                                         &p->asbd,
                                         layout_size, layout,
                                         0, NULL,
                                         NULL,
                                         &p->desc);
    CHECK_CA_ERROR_L(coreaudio_error,
                     "unable to create format description");

#if defined(__IPHONE_15_0) || defined(__MAC_12_0)
    if (@available(tvOS 15.0, iOS 15.0, macOS 12.0, *)) {
        p->renderer.allowedAudioSpatializationFormats = p->spatialize;
    }
#endif

    if (af_fmt_is_spdif(ao->format)) {
        const AVInputFormat *fmt = av_find_input_format("spdif");
        if (!fmt)
            goto coreaudio_error;

        if (!(p->avf = avformat_alloc_context()))
            goto coreaudio_error;

        if (!(p->avf->pb = avio_alloc_context(NULL, 0, 0, NULL, NULL, NULL, NULL)))
            goto coreaudio_error;

        if (avformat_open_input(&p->avf, NULL, fmt, NULL) < 0)
            goto coreaudio_error;

        if (!(p->pkt = av_packet_alloc()))
            goto coreaudio_error;
    }

    ret = true;

coreaudio_error:
    talloc_free(layout);
    return ret;
}

static void avf_start(struct ao *ao)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        struct priv *p = ao->priv;

        [p->synchronizer setRate:1.0];
    }
}

static bool set_pause(struct ao *ao, bool paused)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        struct priv *p = ao->priv;

        [p->synchronizer setRate:paused ? 0.0 : 1.0];

        return true;
    } else {
        return false;
    }
}

static void avf_stop(struct ao *ao)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        struct priv *p = ao->priv;
        [p->synchronizer setRate:0.0 time:CMTimeMake(0, ao->samplerate)];
        [p->renderer flush];
        p->enqueued = 0;
    }
}

static void avf_uninit(struct ao *ao)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        struct priv *p = ao->priv;

        p->renderer = nil;
        p->synchronizer = nil;
        if (p->desc) {
            CFRelease(p->desc);
            p->desc = NULL;
        }

        if (p->avf && p->avf->pb)
            avio_context_free(&p->avf->pb);

        avformat_free_context(p->avf);
        p->avf = NULL;

        av_packet_free(&p->pkt);

#if defined(__IPHONE_14_5) || defined(__MAC_11_3)
        if (@available(tvOS 14.5, iOS 14.5, macOS 11.3, *)) {
            p->synchronizer.delaysRateChangeUntilHasSufficientMediaData = NO;
        }
#endif


#ifdef HAVE_AVAUDIOSESSION
        [AVAudioSession.sharedInstance
            setActive:NO
            withOptions:AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation
            error:nil];
#endif
    }
}

static int avf_init(struct ao *ao)
{
    if (@available(tvOS 12.0, iOS 12.0, macOS 10.14, *)) {
        static_assert(PRIV_SIZE == sizeof(struct priv), "Incorrect PRIV_SIZE value!");

        struct priv *p = ao->priv;

        p->renderer = [AVSampleBufferAudioRenderer new];
        p->synchronizer = [AVSampleBufferRenderSynchronizer new];
        [p->synchronizer addRenderer:p->renderer];

        ao->device_buffer = 2048; // "enough"

        p->enqueued = 0;
        [p->synchronizer setRate:0.0 time:CMTimeMake(0, ao->samplerate)];

        if (!init_renderer(ao))
            return CONTROL_ERROR;

        return CONTROL_OK;
    } else {
        MP_FATAL(ao, "unsupported on this OS version\n");
        return CONTROL_ERROR;
    }
}

static int get_volume(struct ao *ao, float *vol) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct priv *p = ao->priv;
    *vol = p->renderer.volume * 100;
    return CONTROL_TRUE;
}

static int set_volume(struct ao *ao, float *vol) API_AVAILABLE(macos(10.13), ios(11.0))
{
    struct priv *p = ao->priv;
    p->renderer.volume = *vol / 100;
    return CONTROL_TRUE;
}

static int avf_control(struct ao *ao, enum aocontrol cmd, void *arg)
{
    if (@available(tvOS 11.0, iOS 11.0, macOS 10.13, *)) {
        struct priv *p = ao->priv;
        switch (cmd) {
        case AOCONTROL_GET_VOLUME:
            return get_volume(ao, arg);
        case AOCONTROL_SET_VOLUME:
            return set_volume(ao, arg);
        case AOCONTROL_GET_MUTE:
            *(bool*)arg = p->renderer.muted;
            return CONTROL_TRUE;
        case AOCONTROL_SET_MUTE:
            p->renderer.muted = *(bool*)arg;
            return CONTROL_TRUE;
        case AOCONTROL_UPDATE_MEDIA_ROLE:
            set_media_role(*(enum aocontrol_media_role*)arg);
            return CONTROL_TRUE;
        case AOCONTROL_UPDATE_STREAM_TITLE:
            return CONTROL_UNKNOWN;
        }
    }
    return CONTROL_UNKNOWN;
}

static void get_state(struct ao *ao, struct mp_pcm_state *state)
{
    if (@available(tvOS 12.0, iOS 12.0, macOS 10.14, *)) {
        struct priv *p = ao->priv;

        CMTime ctime = [p->synchronizer currentTime];
        bool ready = [p->renderer isReadyForMoreMediaData];
        state->delay = (p->enqueued - CMTimeGetSeconds(ctime));
        state->queued_samples = state->delay * ao->samplerate;
        state->free_samples = ready ? ao->device_buffer : 0;
        state->playing = state->delay >= 0;
    }
}

#define OPT_BASE_STRUCT struct priv

const struct ao_driver audio_out_avfoundation = {
    .description    = "AVFoundation AVSampleBufferAudioRenderer (macOS/iOS)",
    .name           = "avfoundation",
    .uninit         = avf_uninit,
    .init           = avf_init,
    .control        = avf_control,
    .reset          = avf_stop,
    .start          = avf_start,
    .write          = avf_write,
    .set_pause      = set_pause,
    .get_state      = get_state,
#if HAVE_COREAUDIO
    .list_devs      = ca_get_device_list,
#endif
    .priv_size      = PRIV_SIZE,
    .options   = (const struct m_option[]) {
        {"spatialize", OPT_CHOICE(spatialize,
            {"none", AVAudioSpatializationFormatNone},
            {"mono", 0x01UL},
            {"stereo", 0x02UL},
            {"monostereo", AVAudioSpatializationFormatMonoAndStereo},
            {"multichannel", AVAudioSpatializationFormatMultichannel},
            {"all", AVAudioSpatializationFormatMonoStereoAndMultichannel}
        )},
        {0}
    },
    .options_prefix = "avfoundation",
};
