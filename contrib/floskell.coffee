{CompositeDisposable} = require 'atom'
{BufferedProcess} = require 'atom'
{dirname} = require 'path'
{statSync} = require 'fs'

prettify = (args, text, workingDirectory, {onComplete, onFailure}) ->
  lines = []
  proc = new BufferedProcess
    command: 'floskell'
    args: args
    options:
      cwd: workingDirectory
    stdout: (line) -> lines.push(line)
    exit: -> onComplete?(lines.join(''))
  proc.onWillThrowError ({error, handle}) ->
    atom.notifications.addError "Floskell could not spawn",
      detail: "#{error}"
    onFailure?()
    handle()
  proc.process.stdin.write(text)
  proc.process.stdin.end()

prettifyFile = (args, editor, format = 'haskell') ->
  [firstCursor, cursors...] = editor.getCursors().map (cursor) ->
    cursor.getBufferPosition()
  try
    workDir = dirname(editor.getPath())
    if not statSync(workDir).isDirectory()
      workDir = '.'
  catch
    workDir = '.'
  prettify args, editor.getText(), workDir,
    onComplete: (text) ->
      editor.setText(text)
      if editor.getLastCursor()?
        editor.getLastCursor().setBufferPosition firstCursor,
          autoscroll: false
        cursors.forEach (cursor) ->
          editor.addCursorAtBufferPosition cursor,
            autoscroll: false

module.exports = Floskell =
  disposables: null
  menu: null

  activate: (state) ->
    @disposables = new CompositeDisposable
    @menu = new CompositeDisposable

    @disposables.add \
      atom.commands.add 'atom-text-editor[data-grammar~="haskell"]',
        'floskell:prettify-none': ({target}) =>
          prettifyFile [], target.getModel()
        'floskell:prettify-chris-done': ({target}) =>
          prettifyFile ['--style', 'chris-done'], target.getModel()
        'floskell:prettify-cramer': ({target}) =>
          prettifyFile ['--style', 'cramer'], target.getModel()
        'floskell:prettify-gibiansky': ({target}) =>
          prettifyFile ['--style', 'gibiansky'], target.getModel()
        'floskell:prettify-johan-tibell': ({target}) =>
          prettifyFile ['--style', 'johan-tibell'], target.getModel()

    @menu.add atom.menu.add [
      label: 'floskell'
      submenu : [
        {label: 'Default', command: 'floskell:prettify-none'}
        {label: 'Cramer', command: 'floskell:prettify-cramer'}
        {label: 'Chris Done', command: 'floskell:prettify-chris-done'}
        {label: 'Gibiansky', command: 'floskell:prettify-gibiansky'}
        {label: 'Johan Tibell', command: 'floskell:prettify-johan-tibell'}
      ]
    ]

  deactivate: ->
    @disposables.dispose()
    @disposables = null

    @clearMenu()

  clearMenu: ->
    @menu.dispose()
    @menu = null
    atom.menu.update()
