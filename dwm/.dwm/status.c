#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <pulse/pulseaudio.h>
#include <X11/Xlib.h>

static pa_threaded_mainloop *mainloop;
static pa_mainloop_api *mainloop_api;
static pa_context *context;
static char vol_buf[32] = "(no pulseaudio)";
static char mic_buf[32] = "(no pulseaudio)";
static char date_buf[32];
static char time_buf[32];
static char status_buf[1024];

static void
update_status(void)
{
    sprintf(status_buf, " %s %s /  %s /  %s ", mic_buf, vol_buf, time_buf, date_buf);

    Display *dpy = XOpenDisplay(NULL);
    int screen = DefaultScreen(dpy);
    Window root = RootWindow(dpy, screen);
    XStoreName(dpy, root, status_buf);
	XCloseDisplay(dpy);

    fprintf(stdout, "%s\n", status_buf);
    fflush(stdout);
}

static void
sink_info_callback(pa_context *c, const pa_sink_info *i, int eol, void *userdata)
{
    if (i)
    {
        float volume = (float)pa_cvolume_avg(&(i->volume)) / (float)PA_VOLUME_NORM;
        sprintf(vol_buf, "%.0f%%%s", volume * 100.0f, i->mute ? " (muted)" : "");
        update_status();
    }
}

static void
source_info_callback(pa_context *c, const pa_source_info *i, int eol, void *userdata)
{
    if (i)
    {
        sprintf(mic_buf, "%s", i->mute ? " muted / " : "");
        update_status();
    }
}

static void
server_info_callback(pa_context *c, const pa_server_info *i, void *userdata)
{
    pa_context_get_sink_info_by_name(c, i->default_sink_name, sink_info_callback, userdata);
    pa_context_get_source_info_by_name(c, i->default_source_name, source_info_callback, userdata);
}

static void
subscribe_callback(pa_context *c, pa_subscription_event_type_t type, uint32_t idx, void *userdata)
{
    unsigned facility = type & PA_SUBSCRIPTION_EVENT_FACILITY_MASK;

    pa_operation *op = NULL;

    switch (facility)
    {
        case PA_SUBSCRIPTION_EVENT_SINK:
            pa_context_get_sink_info_by_index(c, idx, sink_info_callback, userdata);
            break;

        case PA_SUBSCRIPTION_EVENT_SOURCE:
            pa_context_get_source_info_by_index(c, idx, source_info_callback, userdata);
            break;

        default:
            break;
    }

    if (op)
        pa_operation_unref(op);
}

static void
context_state_callback(pa_context *c, void *userdata)
{
    switch (pa_context_get_state(c))
    {
        case PA_CONTEXT_CONNECTING:
        case PA_CONTEXT_AUTHORIZING:
        case PA_CONTEXT_SETTING_NAME:
            break;

        case PA_CONTEXT_READY:
            pa_context_get_server_info(c, server_info_callback, userdata);

            // Subscribe to sink events from the server. This is how we get
            // volume change notifications from the server.
            pa_context_set_subscribe_callback(c, subscribe_callback, userdata);
            pa_context_subscribe(c, PA_SUBSCRIPTION_MASK_SINK | PA_SUBSCRIPTION_MASK_SOURCE, NULL, NULL);
            break;

        case PA_CONTEXT_TERMINATED:
            mainloop_api->quit(mainloop_api, 0);
            break;

        case PA_CONTEXT_FAILED:
        default:
            fprintf(stderr, "Connection failure: %s\n", pa_strerror(pa_context_errno(c)));
            mainloop_api->quit(mainloop_api, 1);
            break;
    }
}

int main()
{
    mainloop = pa_threaded_mainloop_new();
    mainloop_api = pa_threaded_mainloop_get_api(mainloop);
    context = pa_context_new(mainloop_api, "DWM status");
    pa_context_connect(context, NULL, PA_CONTEXT_NOAUTOSPAWN, NULL);
    pa_context_set_state_callback(context, context_state_callback, NULL);
    pa_threaded_mainloop_start(mainloop);

    while (1)
    {
        time_t timer = time(NULL);
        struct tm* tm_info = localtime(&timer);

        strftime(date_buf, 26, "%Y-%m-%d", tm_info);
        strftime(time_buf, 26, "%I:%M %p", tm_info);

        update_status();

        usleep(10 * 1000 * 1000);
    }

    mainloop_api->quit(mainloop_api, 0);
    pa_threaded_mainloop_stop(mainloop);

    pa_context_unref(context);
    pa_threaded_mainloop_free(mainloop);

    return 0;
}
