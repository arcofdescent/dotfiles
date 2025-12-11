
local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- Appearance
config.font = wezterm.font("JetBrains Mono")
config.font_size = 12.5
config.line_height = 1.1
config.color_scheme = "Tokyo Night Moon"
config.window_decorations = "RESIZE"
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.max_fps = 144
config.animation_fps = 144

-- Tmux-style Keybindings
-- Set a leader key (CTRL-space)
config.leader = { key = ' ', mods = 'CTRL', timeout_milliseconds = 2000 }

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

