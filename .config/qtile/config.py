# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.backend.wayland import InputConfig

from qtile_extras import widget
from qtile_extras.widget.groupbox2 import GroupBoxRule
from qtile_extras.widget.decorations import RectDecoration

from os.path import expanduser
from subprocess import Popen
from libqtile import hook

# What I use for tabbed layout
# https://github.com/hanschen/qtile_tabbed
from tabbed import Tabbed


@hook.subscribe.startup_once
def startup():
    Popen(expanduser("~/.config/qtile/startup.sh"))


mod = "mod1"
terminal = "foot"
browser = "firefox"
launcher = "tofi-drun --drun-launch=true"

BORDER_FOCUS = "d3869b"
BORDER_NORMAL = "83a598"
NO_BORDER = "00000000"
BORDER_WIDTH = 2

TAB_BG = "458588"
TAB_FG = "282828"
ACTIVE_TAB = BORDER_FOCUS
URGENT_TAB = "cc241d"
INACTIVE_TAB = BORDER_NORMAL


keys = [
    # Volume and Brightness keys
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%+"),
    ),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%-"),
    ),
    Key(
        [],
        "XF86AudioMute",
        lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
    ),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +1%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 1%-")),
    # Spawn Browser
    Key([mod], "c", lazy.spawn(browser)),
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key(
        [mod],
        "space",
        lazy.layout.next(),
        desc="Move window focus to other window",
    ),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"],
        "h",
        lazy.layout.shuffle_left(),
        desc="Move window to the left",
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key(
        [mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"
    ),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key(
        [mod, "control"],
        "h",
        lazy.layout.grow_left(),
        desc="Grow window to the left",
    ),
    Key(
        [mod, "control"],
        "l",
        lazy.layout.grow_right(),
        desc="Grow window to the right",
    ),
    Key(
        [mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"
    ),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    # Key(
    #     [mod, "shift"],
    #     "Return",
    #     lazy.layout.toggle_split(),
    #     desc="Toggle between split and unsplit sides of stack",
    # ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key(
        [mod],
        "t",
        lazy.window.toggle_floating(),
        desc="Toggle floating on the focused window",
    ),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    #    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawn(launcher), desc="Spawn the app launcher"),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(
                func=lambda: qtile.core.name == "wayland"
            ),
            desc=f"Switch to VT{vt}",
        )
    )


groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod + shift + group number = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(
                    i.name
                ),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod + shift + group number = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(
        # border_focus_stack=["#d75f5f", "#8f3d3d"],
        border_width=BORDER_WIDTH,
        # margin=[10, 4, 10, 4],
        # border_on_single=True,
        # margin_on_single=[10, 4, 10, 4],
        margin=[0, 0, 0, 0],
        margin_on_single=[0, 0, 0, 0],
        border_focus=BORDER_FOCUS,
        border_normal=NO_BORDER,
    ),
    Tabbed(
        border_width=BORDER_WIDTH,
        margin=0,
        border_focus=BORDER_FOCUS,
        # The border for a normal window does not matter since
        # we can only see one window at a time
        # and all the other ones are tabs
        # border_normal=BORDER_NORMAL,
        # rounded_tabs=True,
        bar_height=25,
        fontsize=13,
        bg_color=TAB_BG,
        active_bg=ACTIVE_TAB,
        urgent_bg=URGENT_TAB,
        inactive_bg=INACTIVE_TAB,
        # They are the same color as the background because they tab is only a thin strip
        # and the color indicates its status, I do not need to know the window name on tab
        active_fg=TAB_FG,
        urgent_fg=TAB_FG,
        inactive_fg=TAB_FG,
    ),
    # layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrains Mono Nerd Font",
    fontsize=13,
    padding=3,
)
extension_defaults = widget_defaults.copy()


# Make long names shorter such as Browsers
def longNameParse(window_name):
    for string in [
        "Chromium",
        "Firefox",
    ]:  # Add any other apps that have long names here
        if string in window_name:
            text = string
            return text

    if len(window_name) >= 57:
        text = window_name[:57]
        return text

    return window_name


screens = [
    Screen(
        wallpaper="~/.config/qtile/wallpapers/background_oxocarbon.png",
        wallpaper_mode="fill",
        top=bar.Bar(
            [
                widget.GroupBox2(
                    padding_x=5,
                    rules=[
                        GroupBoxRule(visible=False).when(occupied=False),
                        GroupBoxRule(block_border_colour="458588").when(
                            focused=True
                        ),
                        GroupBoxRule(block_colour="282828").when(focused=True),
                        GroupBoxRule(text_colour="ffffff").when(focused=True),
                        GroupBoxRule(block_corner_radius=3).when(occupied=True),
                    ],
                ),
                widget.Spacer(5),
                widget.WindowName(parse_text=longNameParse),
                widget.Volume(
                    mute_format="[ Muted]", unmute_format="[  {volume}%]"
                ),
                widget.CPU(format="[CPU  {load_percent}%]"),
                widget.Memory(
                    measure_mem="G",
                    format="[RAM  {MemUsed:.0f}{mm}/{MemTotal:.0f}{mm}]",
                ),
                widget.Battery(
                    format="[ {char} {percent:2.0%}]",
                    charge_char="Charging",
                    discharge_char="Discharging",
                    empty_char="Empty",
                    low_percentage=0.15,
                ),
                widget.Backlight(
                    backlight_name="intel_backlight",
                    format="[󰃠 {percent:2.0%}]",
                ),
                widget.Clock(format="[%a %d %b %Y] [ %I:%M %p]"),
                widget.StatusNotifier(),
            ],
            24,
            background="232323",
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]


dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# Touchpad settings
wl_input_rules = {
    "type:touchpad": InputConfig(tap=True, natural_scroll=True),
    # I use xremap instead
    # "type:keyboard": InputConfig(kb_options="caps:swapescape"),
    # Between -1 and 1
    "type:pointer": InputConfig(pointer_accel=-0.8),
}

# When using the Wayland backend, this can be used to configure input devices.
# wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
