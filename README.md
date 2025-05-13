# opinionized Emacs Configuration

A performance-optimized and well-organized Emacs configuration for Emacs 31, focused on programming and productivity.

## Installation

1. Clone this repository to `~/.config/emacs` (or `~/.emacs.d`):

```bash
git clone https://github.com/4n7l3r/antler-emacs ~/.config/emacs
```

2. Optional: Install external dependencies for best experience:

```bash
# For faster searches
sudo apt-get install ripgrep fd-find      # Ubuntu/Debian
brew install ripgrep fd                   # macOS

# For vterm support
sudo apt-get install libtool-bin cmake    # Ubuntu/Debian
brew install libtool cmake                # macOS

# For language servers
npm install -g typescript-language-server pyright
```

On first start, straight.el will bootstrap itself and download all packages automatically. Necessary directories will be created automatically.

## Performance

This configuration is designed for high performance:

- Optimized GC settings with gcmh
- Tree-sitter for syntax highlighting
- Efficient file handling and backups
- System-specific optimizations
- Child-frame UI enhancements
- LSP optimizations

## Requirements

- Emacs 30+ (best with Emacs 31)
- Git
- Ripgrep & fd (optional, for faster search)
- Language servers for LSP

## Credits

This configuration is inspired by various Emacs distributions including Doom Emacs, Spacemacs, and others.