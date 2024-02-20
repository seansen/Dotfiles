#!/bin/zsh
# .zprofile: Zsh profile file, runs on login. Set environment variables here.

# Set default programs
export BROWSER="firefox"
export EDITOR="nvim"
export TERMINAL="st"

# Define XDG base directory specification
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"  # Uncomment if you want to use this

# Set custom Zsh directory (make sure this directory exists and contains your zsh configurations)
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

# Set paths for specific configurations (uncomment if needed)
#export INPUTRC="${XDG_CONFIG_HOME:-$HOME/.config}/shell/inputrc"
#export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"

# Configure history file settings (uncomment if needed)
#export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/history"
export LESSHISTFILE="-"  # Set to '-' to disable less history (avoiding clutter in home directory)

# Additional environmental settings
export SSB_HOME="$XDG_DATA_HOME/zoom"  # Specific for Zoom, modify according to your needs

# Update PATH environment variable
export PATH="$HOME/.local/bin:$PATH"  # Adds `~/.local/bin` to the beginning of $PATH

# Unset options (if you have any to unset, do it here)
unsetopt PROMPT_SP  # Example: Unset special prompt characters if not needed

# If there are any other environmental variables or initializations you need, set them here
