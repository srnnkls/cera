# The cera guide

This guide explains how a field works and how to use each part of cera. Every function, option
and hook named here is listed with its signature and default in [REFERENCE.md](REFERENCE.md).

## Contents

- [How cera works](#how-cera-works)
- [Reading a line of text](#reading-a-line-of-text)
- [Marking the field](#marking-the-field)
- [Stacking panes around a field](#stacking-panes-around-a-field)
- [Showing panes outside a field](#showing-panes-outside-a-field)
- [Completion](#completion)
- [The child-frame backend](#the-child-frame-backend)
- [Evil and Corfu](#evil-and-corfu)
- [Building on cera](#building-on-cera)
- [When something looks wrong](#when-something-looks-wrong)
- [Where to look next](#where-to-look-next)

## How cera works

A field is made of *panes*, listed top to bottom in a *stack*:

- The *input pane* is where you write. A stack has exactly one.
- A *bounded* read-only pane is a range of lines already in the buffer, such as the region the
  field is about. cera draws a bracket beside those lines and leaves their text alone.
- A *supplied* read-only pane is text you hand to cera, such as context or a status line. It is
  drawn with overlays between the buffer's lines and is never inserted.

`cera-read` builds the common stack: a bounded pane for the region or the current line, followed
by the input pane. `cera-read-stack` takes any stack you build. Both return the text written into
the input, or signal `quit` when the field is cancelled.

`cera-input-backend` decides where the input is written:

- The *buffer backend* inserts a line below the source, lets you edit only that line, and removes
  it when the field closes. Every line below the field moves down while it is open.
- The *frame backend* inserts nothing. It holds rows open below the source with an overlay and
  lays a child frame over them, and the input is written in a buffer of its own inside that
  frame. Lines below the field keep their positions and line numbers.

The default, `auto`, takes the frame on a graphic display and the buffer in a terminal.

With the buffer backend, a field goes through these steps:

1. cera checks the stack and copies the panes, before it changes anything.
2. It inserts the input line inside a change group, with the buffer's undo list set aside.
3. It sets the buffer up for writing: the buffer becomes writable, auto-saving and lock files
   stop, `hl-line-mode` turns off, and edits outside the field are rejected.
4. It turns on `cera-mode`, draws the brackets and panes, and runs `cera-session-start-hook`.
5. A recursive edit runs until you accept or cancel.
6. cera runs `cera-session-teardown-hook`, cancels the change group, restores every setting it
   changed, and runs `cera-session-restored-hook`.

Point, the mark, narrowing, the window configuration, the undo list and the modification flag
come back as they were, whether you accept, cancel, or an error ends the field. While the field is
open the buffer is not shown as modified.

A command that takes the whole buffer cancels the field before it runs: saving, reverting,
killing the buffer, changing its major mode and exiting Emacs. What you wrote goes on the kill
ring, and the command goes ahead on the buffer as it was.

## Reading a line of text

`cera-read` takes a completion table and three optional arguments:

```emacs-lisp
(cera-read TABLE &optional INITIAL BOUNDS SOURCE-FACE)
```

- TABLE is offered by `C-SPC` and `C-r`. Pass nil for a field with nothing to complete.
- INITIAL is the text the field opens with.
- BOUNDS is the source as a cons of positions, `(BEGIN . END)`. Without it, the source is the
  active region, and without a region it is the current line.
- SOURCE-FACE marks the source text, `cera-source` (an underline) by default. Pass nil to leave
  the source unmarked. The current line, used when there is no region or BOUNDS, is never marked.

The field opens below the last line of the source. Cancelling signals `quit`, like the minibuffer,
so a command that should carry on after a cancel catches it:

```emacs-lisp
(defvar my-todos nil)

(defun my-todo ()
  "Write a TODO about the current line and keep it in `my-todos'."
  (interactive)
  (if-let* ((text (condition-case nil
                      (cera-read nil)
                    (quit nil))))
      (push (cons (point-marker) text) my-todos)
    (message "No TODO added")))
```

The field can hold several lines. `RET` accepts, so `C-q C-j` inserts a newline, and text pasted
with its own newlines stays as it is.

Only one field is open in a buffer at a time; opening a second one signals `A field is already
open`.

## Marking the field

The field is drawn with a bracket in the left margin:

```
╭ first line of the source
│ last line of the source
╰ the input
```

Three options change how it looks. Bind them around a call, or set them in a buffer:

- `cera-input-prefix` puts a short label or an icon where the bracket turns into the input. It is
  a string, or a function returning one. The prefix takes `cera-input-prefix-width` columns, 2 by
  default, however wide it is drawn, so the input starts in the same column for any prefix that
  fits.
- `cera-indent` moves the bracket to a column. cera hides the leading whitespace of the bracketed
  lines, so indented text stays where it is and the bracket sits in its indentation. Set it to the
  text's column minus `cera-bracket-width`.
- `cera-space-below` leaves pixels of blank space under the input.

On a line indented by four columns:

```emacs-lisp
(let ((cera-input-prefix "✎")
      (cera-indent 2))
  (cera-read nil))
```

```
  ╭ (message "Hello, %s" name)
  ╰ ✎ ─ the input
```

The input's background is the theme's own with `cera-body-shade` percent of its foreground mixed in,
darker on a light theme and lighter on a dark one. Faces:

- `cera-body` is the field. It inherits `org-block`.
- `cera-border` is the bracket. It inherits `shadow`.
- `cera-source` marks the source text.

The field's text is never coloured by the buffer's major mode. To colour it, set
`cera-input-fontifier` to a function of the field's first and last positions;
`cera-fontify-input-as-markdown` colours it as Markdown when `markdown-mode` is installed.

## Stacking panes around a field

`cera-read-stack` reads a field from a stack you build out of `cera-pane` structs. This one shows
a context line above the current line, the input below it, and an empty status line under the
input:

```emacs-lisp
(let ((line (cons (line-beginning-position) (line-end-position))))
  (cera-read-stack
   (list (cera-pane :id 'context :kind 'readonly :bracket nil
                    :text "Question about this line:")
         (cera-pane :id 'source :kind 'readonly :bounds line :connection 'next)
         (cera-pane :id 'input :kind 'input :text "" :connection 'previous)
         (cera-pane :id 'status :kind 'readonly :bracket nil :align 'input
                    :text ""))
   nil))
```

Where each pane goes:

- Bounded panes stay on their lines. They must follow the buffer's line order and must not share a
  line with each other.
- The input opens below the last bounded pane that precedes it in the stack, or below the current
  line when none does.
- Supplied panes are drawn in stack order between their neighbours: above a bounded pane or the
  input when they precede it, and under the field when they come last.
- A supplied pane with empty text is hidden.

Each pane's slots shape how it is drawn:

- `bracket` draws the pane's own bracket, on by default. `connection` joins it to its neighbour:
  `next` runs the bracket on into the pane below, and `previous` continues it from the pane above.
  `cera-read` joins the source to the input this way.
- `prefix` labels the bracket's corner, at the bottom by default or at the top with
  `prefix-position` set to `top`.
- `face` colours a supplied pane's text, marks a bounded pane's text, or replaces the field's face
  on the input pane.
- `wrap` breaks lines too wide for the window at the last space that fits, on by default. With
  `wrap` nil they are cut short.
- `indent` sets every row in by that many columns, and `max-width` caps a row's width.
- `align` set to `input` starts a pane without a bracket in the column the input's text starts in.

A supplied pane's text is a string, or a list of blocks. A block is `(TEXT . GAP)`, where GAP is
the pixels of blank space kept under TEXT, so a pane can hold paragraphs set apart by less than a
full line.

While the field is open, `cera-update-pane` replaces what a supplied pane shows. Call it in the
buffer the field was opened for, from a timer or a process filter:

```emacs-lisp
(with-current-buffer buffer
  (cera-update-pane 'status "3 files in context"))
```

The input's text, point, undo list and completion are left alone. An empty string hides the pane
again.

To add panes to every field `cera-read` opens, set `cera-read-context-function`. It receives the
stack `cera-read` built and returns the stack to read. `cera-set-pane-text` sets the text of a
pane in that stack, for example to open the input with a saved draft.

## Showing panes outside a field

`cera-pane-show` draws supplied read-only panes by a line with no field open, and leaves them
there until you remove them. It returns a handle for `cera-pane-update` and `cera-pane-remove`:

```emacs-lisp
(setq my-shown
      (cera-pane-show
       (list (cera-pane :id 'note :kind 'readonly :bracket nil :max-width 60
                        :text "Check the error path before merging."))
       (point)))

(cera-pane-update my-shown
                  (list (cera-pane :id 'note :kind 'readonly :bracket nil
                                   :text "Checked.")))
(cera-pane-remove my-shown)
```

Without a column, the panes are drawn under the line POSITION is on, the way a field draws its
supplied panes, with brackets unless a pane sets `bracket` to nil.

With a column, they are drawn beside the line:

```emacs-lisp
(cera-pane-show panes (point) 60)
```

The first row starts at column 60 on the line itself, or one space past the line's end when the
line reaches further. Every following row starts at column 60 on a line of its own. Brackets are
not drawn beside a line. When fewer than 10 columns remain between the column and the window's
edge, the rows are not wrapped.

Either way, each pane's `indent`, `wrap`, `max-width` and `face` apply, and the panes wear the
default face where they have none of their own. The panes follow their line as the text around
it is edited, and are laid out again when a window showing them changes width. Each window
showing the buffer gets its own layout.

## Completion

A field completes through `completion-at-point`. cera installs one completion function in the
field, which calls `cera-completion-function` with the field's bounds and the table the field was
opened with. The default, `cera-complete-with-table`, offers the whole table over the whole field.

`C-SPC` asks for completion. `C-r` runs `cera-recall`, which reads an entry of the table in the
minibuffer and replaces the field's text with it: a list of earlier messages makes a history to
pick from and edit.

To complete on your own terms, set `cera-completion-function` to a function of BOUNDS and TABLE
that returns a `completion-at-point` list, or nil for nothing. limen uses it to complete file
names after `@` and skills after `/`, and falls back to `cera-complete-with-table` elsewhere.

A popup drawn over the buffer can cover the text below the field. `cera-completion-space` keeps
that many lines free under the field while completion is active; a function of no arguments can
supply the number instead. A frontend that measures its own popup calls `cera-reserve-space` with
the lines it needs, and with 0 to give them back. `corfu-cera` does this for Corfu.

## The child-frame backend

`cera-frame.el` holds the frame backend. `cera-input-backend` chooses it:

| Value | Where the input is written |
| --- | --- |
| `auto` | a child frame on a graphic display, a line of the buffer otherwise |
| `frame` | a child frame wherever one can be shown |
| `buffer` | a line of the buffer |

`M-x cera-toggle-input-backend` switches the next field to the other backend.

The frame backend needs a window on a graphic display that shows the buffer. Where there is none,
the field is read in the buffer instead.

What changes with the frame:

- The buffer keeps every line. The rows held open for the input carry the bracket, so another
  window showing the buffer still shows where the field is.
- The input is written in a hidden `text-mode` buffer. It takes over the opening buffer's
  `cera-input-prefix`, `cera-input-prefix-width`, `cera-indent`, `cera-completion-space`,
  `cera-completion-function`, `cera-input-fontifier` and `cera-borrowed-locals`.
- The session hooks and the field's commands run in that buffer. `cera-origin-buffer` returns
  the buffer the field was opened for, from either one.
- Supplied panes that follow the input, up to the next bounded pane, are drawn under the input's
  rows. `cera-update-pane` reaches them from the opening buffer.
- A buffer displayed from inside the field goes to the window the field is read over.
- Global keys with a modifier in `cera-frame-host-modifiers`, `super` by default, run from the
  window the field is read over, so window and workspace commands work as they do in the buffer.
  Selecting that window again returns the keyboard to the field.
- With `cera-frame-scroll-parent` set, the scroll-other-window commands scroll the buffer the field
  is read over.

A workspace manager that restores a layout into the selected frame can put its own buffer into
the child frame. `cera-frame-parameters` tells it to leave the frame alone; the default carries
the flag `persp-mode` reads. When another buffer lands in the frame anyway, cera cancels the field,
puts what you wrote on the kill ring, and shows that buffer in the window the field was read over.

## Evil and Corfu

Load the integrations after the packages they integrate:

```emacs-lisp
(with-eval-after-load 'corfu (require 'corfu-cera))
(with-eval-after-load 'evil (require 'evil-cera))
```

`evil-cera` switches to insert state when a field opens in a buffer with `evil-local-mode`, unless
the buffer is in Emacs state. When the field closes, it returns to the state the buffer was in; a
field opened from visual or operator state returns to normal state, since the field consumed the
selection.

`corfu-cera` turns on `corfu-mode` with automatic completion for the field and asks Corfu to
complete once the field is up. It keeps room under the field for the popup when the popup opens
below point, at most `corfu-cera-space` lines, 10 by default. With the frame backend, the popup is
drawn on the frame under the field rather than inside the child frame. When the field closes,
`corfu-mode` goes back to its earlier state.

## Building on cera

A package that reads text with cera usually needs a few more hooks into the field:

- `cera-session-start-hook` runs with the session once the input is ready, in the buffer the input
  is written in. `cera-session-teardown-hook` runs before the field's text and settings are
  restored, and `cera-session-restored-hook` after. Compare `cera-origin-buffer` with your own
  buffer to act on your fields alone.
- `cera-session-keymap`, and the KEYMAP argument of `cera-read-stack`, add key bindings to the
  field. `cera-mode-map`'s own bindings take precedence over both.
- `cera-input-text`, `cera-input-bounds` and `cera-set-input` read and replace what is written,
  for commands bound in the field.
- `cera-accept` and `cera-cancel` close the field from your own commands.
- `cera-register-borrowed-locals` names buffer-local variables your hooks set, so cera restores
  them with its own when the field closes. `cera-borrowed-locals` does the same for one buffer.

A buffer that redraws itself, such as a dashboard or a report, moves the lines a field is
bracketing. Ask `cera-field-open-p` before redrawing and put the redraw off while it returns
non-nil. When a command moves point out of the field, cera puts it back as far from the end of the
input as it was before the command.

## When something looks wrong

| Message or symptom | Cause |
| --- | --- |
| `Only the field is editable` | a command tried to change text outside the field; the change was refused |
| `A field is already open` | the buffer already has a field; accept or cancel it first |
| `A stack needs exactly one input pane` | the stack passed to `cera-read-stack` has no input pane or more than one |
| `Invalid or duplicate pane` | a pane has no ID, a repeated ID, or a slot outside its allowed values |
| `Bounded panes must follow document line order` | bounded panes are out of order or share a line |
| `Invalid pane bounds` | bounds are not positions or markers in this buffer's accessible part |
| `A read-only pane needs text or bounds` | a read-only pane has neither, or text that is not a string or a list of blocks |
| `Not a supplied read-only pane` | `cera-update-pane` named an input pane, a bounded pane or an unknown ID |
| `No field is open` | a field function ran in a buffer without an open field, or after it closed |
| the text you wrote is gone | a save, revert, kill or mode change cancelled the field; the text is on the kill ring |
| the command after `cera-read` never runs on `C-g` | cancelling signals `quit`; catch it with `condition-case` |
| the field opens in the buffer on a graphic display | `cera-input-backend` is `buffer`, or no graphic window shows the buffer |
| a popup covers the text under the field | raise `corfu-cera-space`, or set `cera-completion-space` for another frontend |
| a workspace manager replaces the field's frame | add the parameter it reads to `cera-frame-parameters` |

## Where to look next

- [REFERENCE.md](REFERENCE.md) lists every function, struct slot, hook, option, face and keymap.
- [README.md](README.md#keys-and-commands) has the field's keys.
- The ERT suites in [test/](test/) exercise each behaviour described here; `cera-test.el` covers
  the field and shown panes, `cera-frame-test.el` the frame backend.
