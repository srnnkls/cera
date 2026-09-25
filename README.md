# cera

Write into a temporary field in another buffer.

## About

cera opens a small writing field inside the buffer you are looking at, right below the lines it
is about, and returns what you typed. A bracket in the margin ties the field to those lines, so
the text you write stays next to the code, the diff or the dashboard entry it refers to. When the
field closes, the buffer is exactly as it was: its text, point, narrowing, undo history and
modification flag come back untouched.

A field works like a minibuffer in the middle of a buffer. `cera-read` blocks until you accept
or cancel, cancelling signals `quit`, and completion runs through `completion-at-point`, so
whichever completion UI the buffer uses draws it. The field also opens in read-only and
`special-mode` buffers, and on a graphic display the input goes into a child frame laid over the
buffer, so no line of the buffer moves while you type.

cera is a library as much as a command. Reach for it when a package needs a short piece of text
tied to a place in a buffer: a note on a region, a comment on a diff line, a message about the
code on screen. Around the field it can show read-only *panes*, such as context, status or
earlier replies, and it can show the same panes under or beside a line with no field open.
[scholia](https://github.com/srnnkls/scholia), for annotations as work items, writes notes and
review comments in cera fields and draws saved notes as cera panes.
[limen](https://github.com/srnnkls/limen), an Emacs interface for agents, writes its agent
messages and inbox notes in cera fields.

## Installation

cera needs Emacs 29.1 or newer. Clone the repository and put it on your `load-path`:

```sh
git clone https://github.com/srnnkls/cera.git ~/.emacs.d/site-lisp/cera
```

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/cera")
(require 'cera)
```

Two optional integrations load on request:

```emacs-lisp
(with-eval-after-load 'corfu (require 'corfu-cera))
(with-eval-after-load 'evil (require 'evil-cera))
```

`corfu-cera` keeps room below the field for Corfu's popup, and `evil-cera` enters insert state
while a field is open. [GUIDE.md](GUIDE.md#evil-and-corfu) describes both.

## Getting started

Put this command in your configuration and evaluate it:

```emacs-lisp
(defun my-note ()
  "Write a note about the region or the current line."
  (interactive)
  (message "Note: %s" (cera-read '("explain" "refactor" "test"))))
```

Select two lines of code and run `M-x my-note`. A field opens under the selection, and a bracket
joins the selected lines to it:

```
╭ (defun greet (name)
│   (message "Hello, %s" name))
╰ explain the format string█
```

Type the note and press `RET`. The field disappears, the buffer is as it was, and `my-note`
echoes what you wrote. `C-g` closes the field instead and quits the command. `C-SPC` completes
from the table the field was opened with, here `explain`, `refactor` and `test`, and `C-r` picks
an entry of the table in the minibuffer.

Without a region, the field opens under the current line.

## Keys and commands

While a field is open, `cera-mode` is on and these keys apply above the buffer's own:

| Key | Command | Does |
| --- | --- | --- |
| `RET` | `cera-accept` | keep what was written; while completion is active, completion keeps `RET` |
| `C-c C-c` | `cera-accept` | keep what was written, even while completion is active |
| `C-c C-k`, `C-g` | `cera-cancel` | discard what was written; `cera-read` signals `quit` |
| `C-SPC` | `completion-at-point` | complete in the field, while no completion is active |
| `C-r` | `cera-recall` | read an entry of the field's table in the minibuffer and write it into the field |

Every other key does what it does in the buffer. In a buffer whose own keymap blocks typing,
such as a `special-mode` buffer, the field borrows `text-mode-map` for as long as it is open.
Since `RET` accepts, `C-q C-j` inserts a newline.

`M-x cera-toggle-input-backend` switches the next field between the child frame and a line of the
buffer.

## Concepts

| Term | Meaning |
| --- | --- |
| *field* | the temporary place a reader writes into, open until it is accepted or cancelled |
| *source* | the lines a field is about, marked with a bracket that runs down to the field |
| *pane* | one part of what a field shows, described by a `cera-pane` struct |
| *input pane* | the one pane of a field that is written in |
| *read-only pane* | a pane that shows existing lines of the buffer (*bounded*) or text you supply (*supplied*) |
| *stack* | the ordered list of panes one field shows, with exactly one input pane |
| *shown panes* | supplied read-only panes drawn under or beside a line with no field open |
| *input backend* | where the input is written: a line of the buffer or a child frame |
| *buffer backend* | the input is a line put into the buffer and taken out again when the field closes |
| *frame backend* | the input is written in a child frame laid over rows the buffer holds open, and the buffer gets no new line |
| *session* | the state of one open field, handed to the session hooks |

## Documentation

- [GUIDE.md](GUIDE.md) explains how a field works and walks through each task, starting at
  [How cera works](GUIDE.md#how-cera-works).
- [REFERENCE.md](REFERENCE.md) lists every public function, struct slot, hook, user option, face,
  keymap and integration.

## Development

Tests and lint run through [Eask](https://emacs-eask.github.io/):

```sh
eask install-deps --dev      # Corfu and Evil, for the integration tests
eask run script test         # ERT suites in test/
eask compile                 # byte-compile; warnings are errors
eask run script checkdoc     # docstring lint
eask run script indent       # indentation lint
eask run script clean-generated
```

Where the code lives:

- `cera.el`: the field, panes, shown panes, completion and the buffer backend.
- `cera-frame.el`: the frame backend.
- `corfu-cera.el`: the Corfu integration.
- `evil-cera.el`: the Evil integration.
- `test/`: one ERT suite per file.
