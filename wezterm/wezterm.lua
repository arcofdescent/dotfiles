
local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Detect the OS
local target = wezterm.target_triple -- e.g. 'x86_64-apple-darwin', 'x86_64-unknown-linux-gnu'

-- Appearance
-- Set platform-specific font
if target:find('apple') then
  -- macOS
  config.font = wezterm.font("JetBrains Mono")
  config.font_size = 12.0
elseif target:find('linux') then
  -- Linux
  config.font = wezterm.font("JetBrains Mono Nerd Font")
  config.font_size = 10.0
else
  -- Default fallback for other OSes (Windows, etc)
  config.font = wezterm.font("JetBrains Mono Nerd Font")
  config.font_size = 10.0
end

config.line_height = 1.1
config.color_scheme = "Tokyo Night Moon"
config.window_decorations = "RESIZE"
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = true
config.max_fps = 144
config.animation_fps = 144

config.window_frame = {
  font_size = 15.0,
  active_titlebar_bg = "#1a1b26",
  inactive_titlebar_bg = "#1a1b26",
}

wezterm.on('format-tab-title', function(tab, tabs, panes, config, hover, max_width)
  local title = "  " .. tab.active_pane.title .. "  "
  return title
end)

-- Tmux-style Keybindings
-- Set a leader key (CTRL-space)
config.leader = { key = '\'', mods = 'CTRL', timeout_milliseconds = 2000 }

config.keys = {
  -- TAB NAVIGATION (New)
  -- Next Tab: CTRL + ]
  { key = ']', mods = 'CTRL', action = wezterm.action.ActivateTabRelative(1) },
  -- Previous Tab: CTRL + [
  { key = '[', mods = 'CTRL', action = wezterm.action.ActivateTabRelative(-1) },

  -- Split pane vertically (Leader + %)
  -- Map: CTRL+SPACE, then SHIFT+5 (key: '5', mods: 'LEADER | SHIFT')
  { key = '%', mods = 'LEADER | SHIFT', action = wezterm.action.SplitVertical },

  -- Split pane horizontally (Leader + ")
  -- Map: CTRL+SPACE, then SHIFT+' (key: ''', mods: 'LEADER | SHIFT')
  -- Note: The single quote (') is the key you press, and the SHIFT gives you the "
  { key = '"', mods = 'LEADER | SHIFT', action = wezterm.action.SplitHorizontal },

  -- Navigate panes with Leader + Arrow keys
  { key = 'h', mods = 'LEADER', action = wezterm.action.ActivatePaneDirection 'Left' },
  { key = 'l', mods = 'LEADER', action = wezterm.action.ActivatePaneDirection 'Right' },
  { key = 'k', mods = 'LEADER', action = wezterm.action.ActivatePaneDirection 'Up' },
  { key = 'j', mods = 'LEADER', action = wezterm.action.ActivatePaneDirection 'Down' },

  -- copy mode
  { key = '[', mods = 'LEADER', action = wezterm.action.ActivateCopyMode },

  -- QUICK SELECT / HINT MODE (New)
  -- Sequence: CTRL+SPACE, then s
  { key = 's', mods = 'LEADER', action = wezterm.action.QuickSelect },
}

return config

