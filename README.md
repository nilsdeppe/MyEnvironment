# My Environment Related Files

In this repository will house my various configurations. Some things are
currently still kept in a private repo elsewhere but as I get time I plan to
move all my setups to this repository. It will likely end up including:
- Dot files in my home directory
- Configurations for software that does not use dot files
- My container setup
- Others that I haven't thought of

Pull requests are welcome, but keep in mind that this is my configuration and I
reserve the right to reject a PR just because I don't like the changes :) Of
course that doesn't mean the changes are not good for other people, so ideally I
will move things in a direction that will allow easy customization for people.

## Emacs Configuration

### Installation

The Emacs configuration installs itself, but first you should set a few options
in it. If you open the `.emacs.el` file you will be able to specify options for
some of the packages used, such as
[EIN](https://github.com/millejoh/emacs-ipython-notebook), whether to use Dvorak
bindings, and whether to enable Evil mode. You'll need to specify some paths,
but most of the configuration should work without any changes.

1. Copy or symbolically link `.emacs.el` to your home directory
2. Start Emacs
3. Restart Emacs
4. Delete `~/.emacs.elc`
5. Restart Emacs three times

Everything except [Jupyter](https://jupyter.org),
[ClangFormat](https://clang.llvm.org/docs/ClangFormat.html),
[Clang-Rename](https://clang.llvm.org/extra/clang-rename.html), and
[YouCompleteMeDaemon](https://github.com/ycm-core/ycmd/) will have
installed itself, as well as any completers for
[lsp-mode](https://github.com/emacs-lsp/lsp-mode) That is, everything that isn't
an external-to-Emacs third party dependency is installed on startup.

### Packages

Here is an overview of the packages I use:
- [Ivy, Counsel, and Swiper](https://github.com/abo-abo/swiper)
  instead of Helm and friends.
- [projectile](https://github.com/bbatsov/projectile) for navigating around
  projects
- [window-numbering](https://github.com/nschum/window-numbering.el) to easily
  navigate panes
- [origami](https://github.com/gregsexton/origami.el) for code folding
- [avy](https://github.com/abo-abo/avy) and
  [zzz-to-char](https://github.com/mrkkrp/zzz-to-char) for quick navigation
  around the buffer
- [visual-regexp-steroids](https://github.com/benma/visual-regexp-steroids.el/)
  for regexp-replacements
- Google Style Guide for C/C++ projects (you may need to change that for your
  projects :) )
- [lsp-mode](https://github.com/emacs-lsp/lsp-mode) for code completion in Python,
  C/C++, and Rust (I currently don't use other languages).
- [company](https://github.com/company-mode/company-mode) for general completion
  (hooking into LSP)
- [flycheck](https://github.com/flycheck/flycheck) for syntax checking
- [string-inflection](https://github.com/akicho8/string-inflection) for changing
  the case of the word-at-point
- [multiple-cursors](https://github.com/magnars/multiple-cursors.el) because
  editing with many cursors is faster
- [ein](https://github.com/millejoh/emacs-ipython-notebook) for Jupyter
  notebooks
- [flyspell](https://www.emacswiki.org/emacs/FlySpell) for spell-check
- [magit](https://magit.vc/),
  [gitgutter](https://github.com/syohex/emacs-git-gutter), and
  [gitignore-mode](https://github.com/magit/git-modes) for git
- [evil-mode](https://github.com/emacs-evil/evil) for vi users (I personally am
  not one, but figured others might find it useful, plus I might learn some day
  :) )
- [evil-collection](https://github.com/emacs-evil/evil-collection) is used to
  supplement evil-mode

I've added an option to the change some of the packages' default shortcuts to be
better-suited for fellow Dvorak users.

### Shortcuts

Here are some of the shortcuts I use a lot. Note that I abbreviate control with
`C` and meta/option/alt with `M`. so `C-c M-n` would mean `control-c` followed
by `meta-n`.

#### Navigation and Cursors

| Shortcut     | Description                                                  |
|:------------:|--------------------------------------------------------------|
| `M-p`        | Add cursor above matching currently highlighted region.      |
| `M-n`        | Add cursor below matching currently highlighted region.      |
| `C-c m a`    | Multiple cursors all like the selected region.               |
| `M-?`        | Mark paragraph.                                              |
| `C-/`        | Undo                                                         |
| `C-h`        | Backspace                                                    |
| `M-h`        | Kill previous word                                           |
| `C-c ;`      | Comment or uncomment region                                  |
| `C-m`        | Enter/newline and indent                                     |
| `C-c g`      | Use Counsel to do a `git grep`                               |
| `C-c r`      | Use Counsel to do a `ripgrep`                                |
| `M-.`        | Find tag at point/LSP goto definition                        |
| `M-t`        | Grep for symbol at point                                     |
| `C-x M-f`    | Projectile find file to find file in project.                |
| `M-s`        | Avy go to visible word that starts with character.           |
| `M-c`        | Avy go to visible two characters.                            |
| `C-M-s`      | Visual regexp forward search.                                |
| `C-M-r`      | Visual regexp reverse search.                                |
| `M-1`, `M-2` | Used to go to windows 1, 2, 3, 4, etc.                       |

#### Basic Editing

| Shortcut     | Description                                                  |
|:------------:|--------------------------------------------------------------|
| `C-c C-w`    | Cut to clipboard.                                            |
| `C-c M-w`    | Copy to clipboard.                                           |
| `C-c C-y`    | Paste from clipboard.                                        |
| `M-z`        | Kill from cursor to character.                               |
| `C-c v r`    | Visual regexp query replace.                                 |
| `C-c v m`    | Visual regexp multiple cursors.                              |
| `C-c c i`    | Cycle the string inflection between cases.                   |
| `C-c c l`    | String inflection to `lowerCamelCase`.                       |
| `C-c c c`    | String inflection to `CamelCase`.                            |
| `C-c c s`    | String inflection to `snake_case`.                           |
| `C-c c u`    | String inflection to `SCREAMING_SNAKE_CASE`.                 |
| `M-g M-s`    | Open Magit status.                                           |
| `M-g M-c`    | Magit checkout.                                              |
| `<f7>`       | Run FlySpell over buffer.                                    |
| `<f8>`       | Jump to previous FlySpell error.                             |
| `<f9>`       | Jump to next FlySpell error.                                 |

#### Programming Modes

| Shortcut     | Description                                                  |
|:------------:|--------------------------------------------------------------|
| `C-c C-c`    | Bring up the compilation command to run.                     |
| `C-c C-k`    | Abort running compilation.                                   |
| `C-c p`      | Prefix for the projectile command map. E.g. `C-c p b`.       |
| `C-c o`      | Prefix for Origami commands.                                 |
| `C-c y`      | Prefix for LSP commands.                                     |
| `C-c C-f`    | ClangFormat region.                                          |
| `C-c c p`    | Clang-Rename symbol at point.                                |
| `C-c c q`    | Clang-Rename qualified identifier.                           |

### Troubleshooting

One of the most reliable ways of fixing weird configuration states that I've
found is deleting `~/.emacs.d` and `~/.emacs.elc` and then starting and
restarting Emacs as described in the [Installation](#Installation) section.
If that does not fix the issue and you can reliable reproduce it, please check
if there already is an issue (possibly closed) or discussion on the [blog
post](https://nilsdeppe.com/posts/emacs-c++-ide2) about what you're seeing. If
there isn't an issue, please feel free to file an issue or add a comment on the
[blog post](https://nilsdeppe.com/posts/emacs-c++-ide2) with steps to
reproduce.

### YouCompleteMe

This setup uses YouCompleteMe (ycm) for code completion in C++. clangd is also
supported via LSP-mode if one wants to use that instead. I have also included my
`.ycm_extra_conf.py` file that I use to preprocess compilation flags to deal
with things like precompiled headers that break YCM. It also sets up the default
flags for compilation. I've had to symlink the file into various project
directories because I haven't managed to get emacs-ycmd to tell ycm to load the
conf file from `~/`.

### Signature verification failed Emacs 26.1

The GNU package signature changed with Emacs 26.3, so Emacs 26.1 and 26.2 cannot
authenticate the GNU package repo giving the following error:
```
Failed to verify signature archive-contents.sig:
No public key for 066DAFCB81E42C40 created at 2020-02-10T14:05:02-0800 using RSA
Command output:
gpg: Signature made Mon 10 Feb 2020 02:05:02 PM PST
gpg:                using RSA key C433554766D3DDC64221BFAA066DAFCB81E42C40
gpg: Can't check signature: No public key
```

One simple workaround for this is to temporarily disable the archive signature
checking by adding:
```elisp
(setq package-check-signature nil)
```
to your Emacs init file. Remember to remove or comment it out after you've done
the core installation! Other options are discussed
[here](http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html).

## License
Unless stated otherwise the code in this repository will be distributed under
the Boost Software License v1. Third party code that I store here will have the
copyright and license added to the beginning of the files.
