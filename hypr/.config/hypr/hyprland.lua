-- Ported from hyprland.conf for Hyprland 0.55+.

local terminal = "ghostty"
local fileManager = "nautilus"
local mainMod = "SUPER"

local function envs(vars)
    for name, value in pairs(vars) do
        hl.env(name, value)
    end
end

envs({
    LC_ALL = "",
    LC_CTYPE = "pt_BR.UTF-8",
    XCOMPOSEFILE = "/home/felipe/.config/hypr/XCompose",
    XDG_CURRENT_DESKTOP = "Hyprland",
    XDG_SESSION_DESKTOP = "Hyprland",
    XDG_SESSION_TYPE = "wayland",
    MOZ_ENABLE_WAYLAND = "1",
    AQ_DRM_DEVICES = "/dev/dri/card1:/dev/dri/card2",
    XCURSOR_SIZE = "24",
    XCURSOR_THEME = "Adwaita",
    HYPRCURSOR_SIZE = "24",
    HYPRCURSOR_THEME = "Adwaita",
    NVD_BACKEND = "direct",
    LIBVA_DRIVER_NAME = "nvidia",
    __GLX_VENDOR_LIBRARY_NAME = "nvidia",
    ELECTRON_OZONE_PLATFORM_HINT = "wayland",
})

hl.monitor({ output = "eDP-1", mode = "1920x1080@165", position = "0x0", scale = 1 })
hl.monitor({ output = "eDP-2", mode = "1920x1080@165", position = "0x0", scale = 1 })
hl.monitor({ output = "DP-3", mode = "1920x1080@144", position = "auto", scale = 1 })
hl.monitor({ output = "DP-1", mode = "1920x1080@144", position = "auto", scale = 1 })
hl.monitor({ output = "HDMI-A-1", mode = "2560x1440@120", position = "1920x0", scale = 1 })

for i = 1, 10 do
    hl.workspace_rule({ workspace = "name:" .. i, monitor = "HDMI-A-1" })
end
hl.workspace_rule({ workspace = "name:alt", monitor = "eDP-1" })

hl.config({
    general = {
        gaps_in = 5,
        gaps_out = 5,
        border_size = 1,
        col = {
            active_border = "rgba(27a1b9ff)",
            inactive_border = "rgba(595959aa)",
        },
        resize_on_border = true,
        allow_tearing = true,
        layout = "scrolling",
    },

    decoration = {
        rounding = 10,
        rounding_power = 2,
        shadow = {
            enabled = true,
            range = 4,
            render_power = 3,
            color = "rgba(1a1a1aee)",
        },
        blur = {
            enabled = true,
            size = 8,
            ignore_opacity = true,
            passes = 3,
            noise = 0.01,
            vibrancy = 0.1696,
        },
    },

    animations = {
        enabled = true,
    },

    master = {
        new_status = "master",
    },

    scrolling = {
        fullscreen_on_one_column = true,
        column_width = 0.6,
        direction = "right",
    },

    misc = {
        force_default_wallpaper = 0,
        disable_hyprland_logo = true,
        focus_on_activate = false,
    },

    input = {
        kb_layout = "us,us(intl)",
        kb_variant = "",
        kb_model = "",
        kb_options = "grp:win_space_toggle",
        kb_rules = "",
        follow_mouse = 0,
        repeat_rate = 60,
        repeat_delay = 200,
        sensitivity = 0,
    },

    cursor = {
        no_hardware_cursors = false,
    },

    ecosystem = {
        no_update_news = true,
    },
})

hl.animation({ leaf = "windows", enabled = true, speed = 2, bezier = "default", style = "gnomed" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 2, bezier = "default" })
hl.animation({ leaf = "fade", enabled = false })

hl.device({
    name = "at-translated-set-2-keyboard",
    kb_options = "caps:swapescape",
})

hl.device({
    name = "pixart-wireless-gaming-mouse-1",
    sensitivity = 0.0,
    accel_profile = "flat",
})

hl.device({
    name = "pixart-wireless-gaming-mouse",
    sensitivity = 0.0,
    accel_profile = "flat",
})

hl.device({
    name = "ven_04f3:00-04f3:32b4-touchpad",
    natural_scroll = true,
})

hl.gesture({
    fingers = 3,
    direction = "horizontal",
    action = "workspace",
})

hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal))
hl.bind("ALT + Space", hl.dsp.exec_cmd("walker"))
hl.bind(mainMod .. " + W", hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + L", hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd("uwsm app -- " .. fileManager))
hl.bind(mainMod .. " + T", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen())
hl.bind(mainMod .. " + J", hl.dsp.window.cycle_next())
hl.bind(mainMod .. " + K", hl.dsp.window.cycle_next({ next = false }))

-- hl.bind("Print", hl.dsp.exec_cmd('grim -g "$(slurp)" - | wl-copy'))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.exec_cmd("hyprpicker -a"), { description = "Pick color" })

hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd('brightnessctl s "+5%"'))
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd('brightnessctl s "5%-"'))
hl.bind(mainMod .. " + V", hl.dsp.exec_cmd("pactl set-source-mute @DEFAULT_SOURCE@ toggle"))
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl -p spotify play-pause"))
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("pactl set-sink-volume @DEFAULT_SINK@ +5%"))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("pactl set-sink-volume @DEFAULT_SINK@ -5%"))
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("pactl set-source-mute @DEFAULT_SOURCE@ toggle"))

for i = 1, 10 do
    local key = i % 10
    hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- hl.bind(mainMod .. " + S", hl.dsp.workspace.toggle_special("magic"))
-- hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
-- hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.bind("ALT + C", hl.dsp.send_shortcut({ mods = "CTRL", key = "Insert" }), { description = "Universal copy" })
hl.bind("ALT + V", hl.dsp.send_shortcut({ mods = "SHIFT", key = "Insert" }), { description = "Universal paste" })
hl.bind("ALT + X", hl.dsp.send_shortcut({ mods = "CTRL", key = "X" }), { description = "Universal cut" })

hl.window_rule({
    match = { class = ".*" },
    suppress_event = "maximize",
})
hl.window_rule({ match = { class = "chromium" }, workspace = "1" })
hl.window_rule({ match = { class = "[Ss]potify" }, workspace = "8 silent", no_initial_focus = true })
hl.window_rule({ match = { class = "discord" }, workspace = "9 silent", no_initial_focus = true })
hl.window_rule({ match = { class = "steam" }, workspace = "10 silent", no_initial_focus = true })
hl.window_rule({ match = { class = "^(cs2)$" }, immediate = true })

hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]", gaps_out = 0, gaps_in = 0 })
hl.window_rule({ match = { workspace = "w[tv1]", float = false }, border_size = 0 })
hl.window_rule({ match = { workspace = "w[tv1]", float = false }, rounding = 0 })
hl.window_rule({ match = { workspace = "f[1]", float = false }, border_size = 0 })
hl.window_rule({ match = { workspace = "f[1]", float = false }, rounding = 0 })

hl.exec_cmd("pkill walker; uwsm app -- walker --gapplication-service")

hl.on("hyprland.shutdown", function()
    hl.exec_cmd("systemctl --user stop xdg-desktop-portal.service xdg-desktop-portal-hyprland.service xdg-desktop-portal-gtk.service")
end)

hl.on("hyprland.start", function()
    hl.dispatch(hl.dsp.focus({ workspace = 1 }))
    hl.exec_cmd("systemctl --user reset-failed xdg-desktop-portal.service xdg-desktop-portal-hyprland.service xdg-desktop-portal-gtk.service")
    hl.exec_cmd("systemctl --user import-environment XCOMPOSEFILE LC_ALL LC_CTYPE")
    hl.exec_cmd("dbus-update-activation-environment --systemd XCOMPOSEFILE LC_ALL LC_CTYPE")
    -- hl.exec_cmd("uwsm app -s s -- noctalia-shell")
    hl.exec_cmd("uwsm app -s s -- wayle panel start")
    hl.exec_cmd("uwsm app -s s -- chromium")
    hl.exec_cmd("uwsm app -s s -- spotify")
    hl.exec_cmd("uwsm app -s s -- discord")

    hl.exec_cmd([[dconf write /org/gnome/desktop/interface/gtk-theme "'Adwaita'"]])
    hl.exec_cmd([[dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"]])
    hl.exec_cmd([[dconf write /org/gnome/desktop/interface/icon-theme "'Adwaita'"]])
    hl.exec_cmd([[dconf write /org/gnome/desktop/interface/cursor-theme "'Adwaita'"]])
end)
