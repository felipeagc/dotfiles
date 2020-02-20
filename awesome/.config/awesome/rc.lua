-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
local lain = require("lain")
local markup = lain.util.markup

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify(
      { 
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors 
      })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify(
        {
          preset = naughty.config.presets.critical,
          title = "Oops, an error happened!",
          text = tostring(err) 
        })
    in_error = false
  end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.floating,
  awful.layout.suit.spiral.dwindle,
}
-- }}}

-- Menu {{{
mymainmenu = awful.menu({
  items = { 
    { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
    { "edit config", editor_cmd .. " " .. awesome.conffile },
    { "restart", awesome.restart },
    { "logout", function() awesome.quit() end },
    { "reboot", "systemctl reboot" },
    { "shutdown", "systemctl poweroff" }
  }
})

mylauncher = awful.widget.launcher({
  image = beautiful.awesome_icon,
  menu = mymainmenu 
})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}


-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
      if client.focus then
        client.focus:move_to_tag(t)
      end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
      if client.focus then
        client.focus:toggle_tag(t)
      end
    end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        c:emit_signal(
            "request::activate",
            "tasklist",
            {raise = true}
        )
      end
    end),
    awful.button({ }, 3, function()
      awful.menu.client_list({ theme = { width = 250 } })
    end),
    awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
    end),
    awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
    end))

local function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, false)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Each screen has its own tag table.
  tags = awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" }, s, awful.layout.layouts[1])

  awful.tag.find_by_name(s, "10").layout = awful.layout.layouts[2]

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
      awful.button({ }, 1, function () awful.layout.inc( 1) end),
      awful.button({ }, 3, function () awful.layout.inc(-1) end),
      awful.button({ }, 4, function () awful.layout.inc( 1) end),
      awful.button({ }, 5, function () awful.layout.inc(-1) end)))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = tasklist_buttons,
    layout  = {
      spacing = 4,
      layout = wibox.layout.fixed.horizontal
    },
    widget_template = {
      {
        {
          {
            {
              id     = 'icon_role',
              widget = wibox.widget.imagebox,
            },
            margins = 2,
            widget  = wibox.container.margin,
          },
          {
            id     = 'text_role',
            widget = wibox.widget.textbox,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        left  = 4,
        right = 4,
        widget = wibox.container.margin
      },
      id     = 'background_role',
      widget = wibox.container.background,
    },
  }

  local xres = beautiful.xresources.get_current_theme()

  s.volume = lain.widget.pulsebar { 
    margins = 3,
    width = 48,
    colors = {
      background = beautiful.dark_green,
      mute_background = beautiful.dark_red,
      unmute = beautiful.green,
      mute = beautiful.red,
    }
  }

  s.volume.bar:buttons(awful.util.table.join(
    awful.button({}, 1, function() -- left click
      os.execute(string.format("pactl set-sink-mute %s toggle", s.volume.device))
      s.volume.update()
    end),
    awful.button({}, 2, function() -- middle click
      awful.spawn("pavucontrol")
    end),
    awful.button({}, 4, function() -- scroll up
      os.execute(string.format("pactl set-sink-volume %s +5%%", s.volume.device))
      s.volume.update()
    end),
    awful.button({}, 5, function() -- scroll down
      os.execute(string.format("pactl set-sink-volume %s -5%%", s.volume.device))
      s.volume.update()
    end)
  ))

  s.bat = lain.widget.bat {
    batteries = {"BAT0", "BAT1"},
    settings = function()
      if bat_now.perc == "N/A" then return end

      local perc = bat_now.perc ~= "N/A" and bat_now.perc .. "%" or bat_now.perc

      if bat_now.ac_status == 1 then
        perc = perc .. " plug"
      end

      widget:set_markup(markup.fg.color(beautiful.yellow, perc .. " " .. bat_now.watt .. "W"))
    end
  }

  s.net = awful.widget.watch(
    "nmcli -c no  -g NAME connection show --active",
    10, -- 10 seconds
    function(widget, stdout)
      for line in stdout:gmatch("[^\r\n]+") do
        widget:set_markup(markup.fg.color(beautiful.green, line))
      end
    end)
  
  s.mem = lain.widget.mem {
    settings = function()
      widget:set_markup(markup.fg.color(
        beautiful.cyan,
        "RAM " .. string.format("%.1f", tonumber(mem_now.used)/1024) .. "GiB"))
    end
  }

  -- Create the wibox
  s.mywibox = awful.wibar({
    position = "top",
    screen = s,
    height = beautiful.menu_height,
    border_width = 6
  })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    expand = "none",
    {
      -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      spacing        = 12,

      s.mytaglist,
      s.mytasklist,
    },
    -- Middle widget
    nil, 
    {
      -- Right widgets
      layout = wibox.layout.fixed.horizontal,

      spacing        = 12,

      s.bat.widget,
      s.net,
      s.mem,
      wibox.widget.textclock(
        markup(beautiful.blue, "%A, %B %d   ") ..
        markup(beautiful.magenta, "%H:%M")),
      s.volume.bar,
      wibox.widget.systray(),
      s.mylayoutbox,
      mylauncher,
    },
  }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
    awful.key({ modkey }, "s",      hotkeys_popup.show_help,
              { description = "show help", group="awesome" }),
    awful.key({ modkey }, "Left",   awful.tag.viewprev,
              { description = "view previous", group = "tag" }),
    awful.key({ modkey }, "Right",  awful.tag.viewnext,
              { description = "view next", group = "tag" }),
    awful.key({ modkey }, "Escape", awful.tag.history.restore,
              { description = "go back", group = "tag" }),

    -- Layout manipulation
    awful.key({ modkey }, "u", awful.client.urgent.jumpto,
              { description = "jump to urgent client", group = "client" }),
    awful.key(
        { modkey }, "Tab",
        function ()
          awful.client.focus.history.previous()
          if client.focus then
            client.focus:raise()
          end
        end,
        { description = "go back", group = "client" }),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              { description = "open a terminal", group = "launcher" }),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              { description = "reload awesome", group = "awesome" }),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              { description = "quit awesome", group = "awesome" }),

    awful.key({ modkey,           }, "l",     function () awful.client.focus.bydirection("right") end,
              { description = "focus right", group = "layout" }),
    awful.key({ modkey,           }, "h",     function () awful.client.focus.bydirection("left") end,
              { description = "focus left", group = "layout" }),
    awful.key({ modkey,           }, "j",     function () awful.client.focus.bydirection("down") end,
              { description = "focus down", group = "layout" }),
    awful.key({ modkey,           }, "k",     function () awful.client.focus.bydirection("up") end,
              { description = "focus up", group = "layout" }),

    awful.key({ modkey, "Shift"   }, "l",     function () awful.client.swap.bydirection("right") end,
              { description = "swap right", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.client.swap.bydirection("left") end,
              { description = "swap left", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "j",     function () awful.client.swap.bydirection("down") end,
              { description = "swap down", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "k",     function () awful.client.swap.bydirection("up") end,
              { description = "swap up", group = "layout" }),

    awful.key({ modkey,           }, "b",     function () awful.layout.inc( 1)                end,
              { description = "select next", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "b",     function () awful.layout.inc(-1)                end,
              { description = "select previous", group = "layout" }),

    -- awful.key(
    --     { modkey, "Control" }, "n",
    --     function ()
    --       local c = awful.client.restore()
    --       -- Focus restored client
    --       if c then
    --         c:emit_signal(
    --             "request::activate", "key.unminimize", {raise = true}
    --         )
    --       end
    --     end,
    --     { description = "restore minimized", group = "client" }),

    -- Prompt
    awful.key(
        { modkey }, "space", function () awful.spawn("rofi -show drun") end,
        { description = "rofi", group = "launcher" }),
    awful.key(
        { modkey , "Shift" }, "space", function () awful.spawn("rofi -show run") end,
        { description = "rofi (raw)", group = "launcher" }),

    -- Screenshot
    awful.key(
        { }, "Print", function () awful.spawn("flameshot gui") end,
        { description = "flameshot", group = "launcher" }),

    awful.key(
        { modkey }, "v", function()
          awful.spawn("pactl set-source-mute @DEFAULT_SOURCE@ toggle")
          awful.spawn("notify-send \"Mic mute toggled\"")
        end,
        { description = "toggle mic", group = "launcher" }),

    -- Extra keys
    awful.key(
        {}, "XF86MonBrightnessUp", function() awful.spawn("xbacklight -inc 10") end,
        { description = "increase screen brightness", group = "extra" }),
    awful.key(
        {}, "XF86MonBrightnessDown", function() awful.spawn("xbacklight -dec 10") end,
        { description = "decrease screen brightness", group = "extra" }),

    awful.key(
        {}, "XF86AudioRaiseVolume", function() awful.spawn("pactl set-sink-volume 0 +5%") end,
        { description = "increase volume", group = "extra" }),
    awful.key(
        {}, "XF86AudioLowerVolume", function() awful.spawn("pactl set-sink-volume 0 -5%") end,
        { description = "decrease volume", group = "extra" }),
    awful.key(
        {}, "XF86AudioMute", function() awful.spawn("pactl set-sink-mute 0 toggle") end,
        { description = "mute volume", group = "extra" })
)

clientkeys = gears.table.join(
    awful.key({ modkey }, "f",
              function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
              end,
              {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey,           }, "w",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey,           }, "t",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey, "Shift"   }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
              function (c)
                c.maximized = not c.maximized
                c:raise()
              end ,
              {description = "(un)maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 10 do
  globalkeys = gears.table.join(
      globalkeys,
      -- View tag only.
      awful.key(
          { modkey }, "#" .. i + 9,
          function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
              tag:view_only()
            end
          end,
          {description = "view tag #"..i, group = "tag"}),
      -- Move client to tag.
      awful.key(
          { modkey, "Shift" }, "#" .. i + 9,
          function ()
            if client.focus then
              local tag = client.focus.screen.tags[i]
              if tag then
                client.focus:move_to_tag(tag)
              end
            end
          end,
          {description = "move focused client to tag #"..i, group = "tag"})
  )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
      awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
      awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  { 
    rule = { },
    properties = {
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal,
      focus = awful.client.focus.filter,
      raise = true,
      keys = clientkeys,
      buttons = clientbuttons,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  },

  -- Floating clients.
  {
    rule_any = {
      instance = {
        "DTA",  -- Firefox addon DownThemAll.
        "copyq",  -- Includes session name in class.
        "pinentry",
      },
      class = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "MessageWin",  -- kalarm.
        "Sxiv",
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
        "Wpa_gui",
        "veromix",
        "xtightvncviewer",
        "Motor"
      },

      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name = {
        "Event Tester",  -- xev.
      },
      role = {
        "AlarmWindow",  -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
        "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
      }
    },
    properties = { floating = true }
  },

  -- Add titlebars to normal clients and dialogs
  {
    rule_any = {
      type = { "normal", "dialog" }
    }, 
    properties = { titlebars_enabled = false }
  },

  -- Set Firefox to always map on the tag named "2" on screen 1.
  {
    rule = { class = "Chromium" },
    properties = { screen = 1, tag = "1" }
  },
  {
    rule = { class = "discord" },
    properties = { screen = 1, tag = "9" }
  },
  {
    rule = { class = "Steam" },
    properties = { screen = 1, tag = "10" }
  },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup
  and not c.size_hints.user_position
  and not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c)
  end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

screen.connect_signal("arrange", function (s)
    local max = s.selected_tag.layout.name == "max"
    local floating = s.selected_tag.layout.name == "floating"
    local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
    -- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
    for _, c in pairs(s.clients) do
      if not floating and (max or only_one) and not c.floating or c.maximized then
        c.border_width = 0
      else
        c.border_width = beautiful.border_width
      end
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

awful.spawn.once("xrdb ~/.Xresources")
awful.spawn.once("xset r rate 220 60")
awful.spawn.once("xsetroot -cursor_name left_ptr")
awful.spawn.with_shell(gears.filesystem.get_configuration_dir() .. "autorun.sh")

-- }}}
