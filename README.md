# Dotfiles

Run `chezmoi apply` to install.

If you change any dotfiles on the home directory, run `chezmoi re-add` to sync them back to this repo.

## Bootstrapping

```bash
brew install chezmoi
chezmoi init --apply https://github.com/felipeagc/dotfiles.git
```
