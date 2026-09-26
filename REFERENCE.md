# cera reference

Every public function, struct, hook, variable, user option, face and keymap, by file. For how
they fit together, read [GUIDE.md](GUIDE.md).

## Contents

- [Files](#files)
- [Reading a field](#reading-a-field)
- [Panes](#panes)
- [The open field](#the-open-field)
- [Shown panes](#shown-panes)
- [Completion](#completion)
- [Hooks and variables](#hooks-and-variables)
- [User options](#user-options)
- [Faces](#faces)
- [Keymap and mode](#keymap-and-mode)
- [Child-frame backend](#child-frame-backend)
- [Corfu integration](#corfu-integration)
- [Evil integration](#evil-integration)

## Files

| File | Feature | Requires | Holds |
| --- | --- | --- | --- |
| `cera.el` | `cera` | Emacs 29.1 | the field, panes, shown panes, completion and the buffer backend |
| `cera-frame.el` | `cera-frame` | `cera` | the frame backend; autoloaded through `cera-frame-read-stack` |
| `corfu-cera.el` | `corfu-cera` | `cera`, Corfu 1.0 | the Corfu integration; takes effect when loaded |
| `evil-cera.el` | `evil-cera` | `cera`, Evil 1.14.0 | the Evil integration; takes effect when loaded |

Unloading `corfu-cera` or `evil-cera` with `unload-feature` removes their hooks and advice.

## Reading a field

### cera-read

```emacs-lisp
(cera-read TABLE &optional INITIAL BOUNDS SOURCE-FACE)
```

Read text into a field below BOUNDS, completing on TABLE, and return it.

| Argument | Meaning |
| --- | --- |
| TABLE | completion table for `C-SPC` and `C-r`, or nil |
| INITIAL | text the field opens with; default empty |
| BOUNDS | the source as `(BEGIN . END)`; default the active region, else the current line |
| SOURCE-FACE | face marking the source; default `cera-source`; nil marks nothing |

The source is marked only when it comes from BOUNDS or the region. `cera-read` builds a stack of
a bounded pane with ID `source` and connection `next`, and an input pane with ID `input`,
connection `previous` and `cera-input-prefix` as its prefix. It passes the stack through
`cera-read-context-function` and reads it with `cera-read-stack`.

Cancelling signals `quit`. Saving, reverting or killing the buffer, changing its major mode or
exiting Emacs cancels the field first and puts its text on the kill ring.

### cera-read-stack

```emacs-lisp
(cera-read-stack PANES TABLE &optional KEYMAP)
```

Read the single input of PANES, a list of `cera-pane` structs, completing on TABLE, and return
the input string. KEYMAP and `cera-session-keymap` add bindings under `cera-mode-map`.
`cera-input-backend` chooses the reader: `cera-frame-read-stack` or `cera-read-stack-in-buffer`.
Cancelling signals `quit`.

The stack is checked before anything changes:

- Every pane has a non-nil ID, unique in the stack.
- `kind` is `input` or `readonly`, `bracket` is t or nil, `prefix-position` is `top` or `bottom`,
  `align` is nil or `input`, and `prefix` is nil or a string.
- Exactly one pane is an input pane. It has no `bounds`, and its `text` is nil or a string.
- A read-only pane has `bounds` and no `text`, or `text` that is a string or a list of blocks.
- `bounds` are integers or markers of the current buffer, within its accessible part.
- Bounded panes follow the buffer's line order and do not share lines.

Placement is described in [GUIDE.md](GUIDE.md#stacking-panes-around-a-field).

### cera-read-stack-in-buffer

```emacs-lisp
(cera-read-stack-in-buffer PANES TABLE &optional KEYMAP)
```

The buffer backend: `cera-read-stack` with the input written into a line inserted into the
buffer and removed before it returns.

### cera-frame-read-stack

```emacs-lisp
(cera-frame-read-stack PANES TABLE &optional KEYMAP)
```

The frame backend, in `cera-frame.el`, autoloaded. See [Child-frame backend](#child-frame-backend).

## Panes

### cera-pane

```emacs-lisp
(cera-pane &key ID KIND TEXT BOUNDS BRACKET PREFIX PREFIX-POSITION FACE
           CONNECTION WRAP ALIGN INDENT MAX-WIDTH)
```

A `cl-defstruct`. `cera-pane` is the constructor, `cera-pane-p` the predicate, `copy-cera-pane`
the copier, and `cera-pane-SLOT` reads a slot.

| Slot | Default | Meaning |
| --- | --- | --- |
| `id` | nil | name of the pane, unique in its stack; `cera-update-pane` finds the pane by it |
| `kind` | nil | `input` or `readonly` |
| `text` | nil | a supplied pane's text: a string or a list of blocks; the input pane's initial text |
| `bounds` | nil | a bounded pane's range, `(BEGIN . END)` |
| `bracket` | t | draw the pane's bracket; nil shows the text alone |
| `prefix` | nil | string labelling the bracket's corner |
| `prefix-position` | `bottom` | the corner the prefix labels: `top` or `bottom` |
| `face` | nil | face of a supplied pane's text, the mark on a bounded pane's text, or the input's face in place of the field's |
| `connection` | nil | `next` runs the bracket on into the pane below; `previous` continues it from the pane above |
| `wrap` | t | break a line too wide for the window at the last space that fits; nil cuts it short |
| `align` | nil | `input` starts a pane without a bracket in the column the input's text starts in |
| `indent` | 0 | columns every row of the text is set in, wrapped rows included |
| `max-width` | nil | columns a row may take, its indent included; nil lets the window decide |

A block is `(TEXT . GAP)`: a string, and a natural number of pixels of blank space kept under it.
A pane whose `text` is a string is one block with no gap. A block with empty text and a gap stands
as a blank line; one with neither is dropped.

A supplied read-only pane with empty text is hidden.

### cera-set-pane-text

```emacs-lisp
(cera-set-pane-text PANE TEXT)
```

Set PANE's `text` to TEXT and return PANE. It lets code that does not load cera at compile time
set the text of a pane it was handed, as `cera-read-context-function` does.

### cera-bracket-width

Constant: the columns the bracket takes before the text it opens, 2.

## The open field

These act on the field open in the current buffer.

| Function | Does |
| --- | --- |
| `(cera-field-open-p &optional BUFFER)` | non-nil while a field is open over BUFFER, the current buffer by default; true in the buffer the field was opened for with either backend; autoloaded |
| `(cera-origin-buffer)` | the buffer the open field was opened for; with the frame backend, called in the input's buffer, the buffer it is read over |
| `(cera-input-text)` | what is written in the field, or nil when none is open |
| `(cera-input-bounds)` | the input's `(BEGIN . END)` positions in the buffer it is written in, or nil |
| `(cera-set-input TEXT)` | replace the field's text with TEXT and put point at its end |
| `(cera-update-pane ID TEXT)` | replace the supplied read-only pane ID with TEXT, a string or a list of blocks; return t |
| `(cera-reserve-space LINES)` | keep LINES empty lines under the field; 0 gives them back |

`cera-input-text`, `cera-input-bounds` and `cera-set-input` work from the buffer the field was
opened for and from the buffer its input is written in.

`cera-update-pane` leaves the input's text, point, undo list and completion alone, and empty TEXT
hides the pane. It signals `user-error` when no field is open, and for an input pane, a bounded
pane, or text that is neither a string nor a list of blocks. For an ID the open field does not
hold, it runs `cera-update-pane-functions` and signals `user-error` when none of them takes the
update.

Interactive commands, bound in `cera-mode-map`:

| Command | Does |
| --- | --- |
| `cera-accept` | keep what was written and close the field |
| `cera-cancel` | discard what was written and close the field; the reader signals `quit` |
| `cera-recall` | read an entry of the field's table in the minibuffer and replace the field's text with it |

## Shown panes

### cera-pane-show

```emacs-lisp
(cera-pane-show PANES POSITION &optional COLUMN)
```

Show the read-only PANES, `cera-pane` structs with supplied text, by the line POSITION is on, and
return a `cera-shown` handle. No field needs to be open.

Without COLUMN, the panes are drawn under the line, as a field draws its supplied panes. With
COLUMN, they are drawn beside it: the first row from COLUMN on the line itself, or one space past
its end when the line reaches further, and every following row from COLUMN on a line of its own.
Beside a line, brackets are not drawn, and rows are not wrapped when fewer than 10 columns remain
right of COLUMN. Each pane's `indent`, `wrap`, `max-width` and `face` apply either way.

The panes follow the line as the buffer is edited and are laid out again when a window showing
them changes width, one layout per window.

### cera-pane-update

```emacs-lisp
(cera-pane-update SHOWN PANES)
```

Show PANES in place of what SHOWN shows, and return SHOWN.

### cera-pane-remove

```emacs-lisp
(cera-pane-remove SHOWN)
```

Take SHOWN's panes off the display.

### cera-shown

The handle `cera-pane-show` returns, a `cl-defstruct` with read accessors `cera-shown-SLOT` and
the predicate `cera-shown-p`.

| Slot | Meaning |
| --- | --- |
| `buffer` | the buffer the panes are shown in |
| `anchor` | marker on the line the panes are shown by |
| `panes` | the panes shown |
| `overlays` | the overlays drawing them, one per window |
| `widths` | the window widths the overlays were laid out for |
| `column` | the column for beside placement, or nil for under |

## Completion

| Name | Kind | Meaning |
| --- | --- | --- |
| `cera-completion-function` | option | function of BOUNDS and TABLE returning a `completion-at-point` list, or nil for none; default `cera-complete-with-table` |
| `(cera-complete-with-table BOUNDS TABLE)` | function | offer TABLE across all of BOUNDS, with `:exclusive no` and `:company-prefix-length 0` |
| `cera-completion-space` | option | lines kept free under the field while completion is active; see [User options](#user-options) |

In the field, `completion-at-point-functions` holds only cera's own function, which calls
`cera-completion-function` when point is inside the field.

## Hooks and variables

| Variable | Called with | Meaning |
| --- | --- | --- |
| `cera-session-start-hook` | SESSION | runs once the input is ready, in the buffer the input is written in |
| `cera-session-teardown-hook` | SESSION | runs before the field's text and settings are restored |
| `cera-session-restored-hook` | SESSION | runs after they are restored |
| `cera-update-pane-functions` | ID, TEXT | runs for a pane the open field does not hold, until one function returns non-nil |
| `cera-read-context-function` | PANES | nil, or a function returning the stack `cera-read` reads, before the buffer changes |
| `cera-session-keymap` | | nil, or a keymap added to the field's bindings, under `cera-mode-map` |
| `cera-borrowed-locals` | | buffer-local list of further variables to restore when a field closes |

SESSION is an opaque value; compare it with `eq`. Every function on the teardown and restored
hooks runs even when one signals, and the first error is signalled again afterwards.

```emacs-lisp
(cera-register-borrowed-locals &rest SYMBOLS)
```

Add SYMBOLS to the default value of `cera-borrowed-locals`, so every field restores their
buffer-local values when it closes. The integrations register their own variables this way.

## User options

Customization group `cera`.

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `cera-input-backend` | `auto`, `frame` or `buffer` | `auto` | where the input is written; `auto` takes the frame on a graphic display |
| `cera-input-prefix` | nil, string or function | nil | label or icon where the bracket turns into the input; a function returns the string |
| `cera-input-prefix-width` | natural number | 2 | columns kept for the prefix, however wide it is drawn; a wider prefix pushes the bracket along |
| `cera-indent` | natural number or function | 0 | column the bracket is drawn at, counted from the window's text area; the leading whitespace of bracketed lines is hidden |
| `cera-body-shade` | natural number | 6 | percent of the theme's foreground mixed into the field's background: darker on a light theme, lighter on a dark one |
| `cera-space-below` | natural number | 0 | pixels left empty under the input |
| `cera-completion-function` | function | `cera-complete-with-table` | see [Completion](#completion) |
| `cera-completion-space` | natural number or function | 0 | lines kept free under the field while completion is active; a function of no arguments returns the number |
| `cera-input-fontifier` | nil or function | nil | function of the field's first and last positions that puts `face` properties on its text; nil leaves it plain |

`cera-fontify-input-as-markdown` is a ready `cera-input-fontifier`: it colours the field as
Markdown in a hidden buffer and copies the faces over. It does nothing without `markdown-mode`.

Options for the frame backend are under [Child-frame backend](#child-frame-backend), and
`corfu-cera-space` under [Corfu integration](#corfu-integration).

## Faces

| Face | Default | Used for |
| --- | --- | --- |
| `cera-body` | inherits `org-block`, extends to the window's edge | the input, from a column before its text; its background is shaded by `cera-body-shade` |
| `cera-border` | inherits `shadow` | the bracket |
| `cera-source` | underline | the source text `cera-read` marks by default |

## Keymap and mode

`cera-mode` is the minor mode on while a field is open, with lighter ` field`. Its keymap,
`cera-mode-map`, takes precedence over the buffer's own keymaps and minor mode maps, followed by
the KEYMAP passed to `cera-read-stack` and `cera-session-keymap`.

| Key | Binding |
| --- | --- |
| `RET`, `<return>` | `cera-accept`, while no completion is active |
| `C-c C-c` | `cera-accept` |
| `C-c C-k` | `cera-cancel` |
| `C-g` | `cera-cancel` |
| `C-SPC` | `completion-at-point`, while no completion is active |
| `C-r` | `cera-recall` |

In a buffer derived from `special-mode`, or one whose keymap remaps `self-insert-command` to
`undefined` or `ignore`, the field uses `text-mode-map` as the local map until it closes.

## Child-frame backend

`cera-frame.el`. The input is written in a child frame over rows the buffer holds open, and the
buffer gets no new line. Where no window on a graphic display shows the buffer, the field is read
in the buffer instead. [GUIDE.md](GUIDE.md#the-child-frame-backend) describes the behaviour.

| Name | Kind | Default | Meaning |
| --- | --- | --- | --- |
| `cera-input-backend` | option, in `cera.el` | `auto` | `auto`, `frame` or `buffer` |
| `cera-toggle-input-backend` | command, in `cera.el` | | make the next field use the other backend than this display would now; a field already open stays as it is |
| `cera-frame-host-modifiers` | option | `(super)` | modifiers, from `super`, `hyper` and `alt`, whose global bindings run from the window the field is read over; a binding `delete-selection-mode` marks as inserting text still writes into the field |
| `cera-frame-scroll-parent` | option | nil | non-nil makes the scroll-other-window commands scroll the window the field is read over |
| `cera-frame-parameters` | option | `((persp-ignore-wconf . t))` | extra parameters for the child frame; they take precedence over cera's own |
| `cera-frame-input-max-width` | option | 80 | columns the input's text takes before it wraps, and where its face ends; it still reaches as far as the widest supplied pane stacked with it; nil runs to the window's edge |

The input's buffer takes these values from the buffer the field is opened for:
`cera-input-prefix`, `cera-input-prefix-width`, `cera-indent`, `cera-completion-space`,
`cera-completion-function`, `cera-input-fontifier` and `cera-borrowed-locals`.

## Corfu integration

`corfu-cera.el`. Loading it adds its functions to `cera-session-start-hook` and
`cera-session-teardown-hook` and advises `corfu--popup-show`.

While a field is open, `corfu-mode` is on in the input's buffer with `corfu-auto` t,
`corfu-auto-prefix` 1, `corfu-auto-trigger` empty and `corfu-quit-at-boundary` t, and Corfu is
asked to complete once the field is up. When the popup opens below point, the field keeps room
for it under the input. With the frame backend, the popup is drawn on the parent frame at the same
place on the screen. `corfu-mode` returns to its earlier state when the field closes.

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `corfu-cera-space` | natural number | 10 | most lines kept free under the field for Corfu's popup |

## Evil integration

`evil-cera.el`. Loading it adds its functions to `cera-session-start-hook`,
`cera-session-teardown-hook` and `cera-session-restored-hook`.

In a buffer with `evil-local-mode`, a field enters insert state unless the buffer is in Emacs
state. When the field closes, the buffer returns to the state it was in, or to normal state when
the field opened from visual or operator state.
