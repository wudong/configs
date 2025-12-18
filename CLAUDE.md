# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal configuration repository for storing various application configuration files used on macOS. The repository contains active configurations for development tools and applications, organized by category.

## Configuration File Organization

When adding configuration files, follow these patterns:

- **Application-specific configs**: Place in appropriate subdirectories (e.g., `vscode/`, `zsh/`, `git/`)
- **Descriptive filenames**: Use descriptive names without leading dots (e.g., `zshrc` instead of `.zshrc`)
- **Documentation**: Add brief comments explaining key configuration choices
- **macOS-specific**: Note any macOS-specific settings or paths

## Configuration File Management

This repository uses a version-and-link approach:

1. **Store configs in repo**: Keep versioned copies in application-specific directories
2. **Create symbolic links**: Link configs to their original system locations
3. **Use setup scripts**: Create shell scripts to automate linking process


Linking commands:
```bash
# Create symbolic link for zsh configuration
ln -s ~/configs/zsh/zshrc ~/.zshrc

# Link git configuration
ln -s ~/configs/git/gitconfig ~/.gitconfig

# Link VSCode settings
ln -s ~/configs/vscode/settings.json ~/Library/Application\ Support/Code/User/settings.json

# Link Emacs configuration
ln -s ~/configs/emacs/init.el ~/.emacs.d/init.el

# Link tmux configuration
ln -s ~/configs/tmux/tmux.conf ~/.tmux.conf
```

