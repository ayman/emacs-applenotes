# applenotes.el
Use Apple Notes in Emacs through Applescript.

*This is working but still in progress yo.* A couple of things still not working:

- Can't make a new note.
- Can't view inline images/attachments.
- Can't open protected note.
- List styles are lost (check list, bullet, etc.)
- Not in Melpa (yet).
- Clean up the duplicated code.

## Dependencies
- Applescript
- Markdown-mode

## Usage
Open a note from the commands below and just `C-x C-s` save it as normal.

### Commands
- `M-x applenotes-all-notes` Get a list of all notes, folders, and accounts.
- `M-x applenotes-accounts-list` Get a list of all accounts and their folders.

### Keybindings
This package does not define any keybindings at the moment. Feel free to define
your own keybindings for each command, like so:

```
(global-set-key (kbd "C-c n f") 'applenotes-all-notes)
(global-set-key (kbd "C-c n l") 'applenotes-all-accounts)
```

## Alternatives/Inspiration
You can check
out
[emacs-geeknote](https://github.com/avendael/emacs-geeknote) if
you use evernote/geeknote.  I used to use it before evernote changed
its policy. Also I took some of the code I contributed there to make this package.
