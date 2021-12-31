Emacs for You (Emfy)
====================

This project provides a tiny [`.emacs`] file to set up Emacs quickly.
This document provides a detailed description of how to set it up and
get started with Emacs.

[![View Source][Source SVG]][Source URL]
[![MIT License][License SVG]][L]
[![Twitter][Twitter SVG]][Twitter URL]

[Source SVG]: https://img.shields.io/badge/view-~%2f.emacs-brightgreen
[Source URL]: .emacs
[License SVG]: https://img.shields.io/badge/license-MIT-%233ea639
[Twitter SVG]: https://img.shields.io/badge/twitter-%40susam-%231da1f2
[Twitter URL]: https://twitter.com/susam
[`.emacs`]: .emacs
[`em`]: em

Further this project also provides a tiny convenience command named
[`em`] to start Emacs server and edit files using Emacs server. This
helps in using Emacs efficiently. This script and its usage is
explained in detail later in the [Emacs Server](#emacs-server) and
[Emacs Launcher](#emacs-launcher) sections. Here is how the Emacs
environment is going to look after setting up this project:

[![Screenshot of Emacs][demo-screenshot]][demo-screenshot]

[demo-screenshot]: https://i.imgur.com/Bp562nt.png

If you are already comfortable with Emacs and only want to understand
the content of [`.emacs`] or [`em`], you can skip ahead directly to
the [Line-by-Line Explanation](#line-by-line-explanation) section that
describes every line of these files in detail.


Contents
--------

* [Who Is This For?](#who-is-this-for)
* [What About Programming in CL?](#what-about-programming-in-cl)
* [Features](#features)
* [Get Started](#get-started)
* [Step-by-Step Usage](#step-by-step-usage)
  * [Use Emacs](#use-emacs)
  * [Use Paredit](#use-paredit)
  * [Execute Emacs Lisp Code](#execute-emacs-lisp-code)
  * [Use Rainbow Delimiters](#use-rainbow-delimiters)
* [Line-by-Line Explanation](#line-by-line-explanation)
  * [Tweak UI Elements](#tweak-ui-elements)
  * [Customize Theme](#customize-theme)
  * [Minibuffer Completion](#minibuffer-completion)
  * [Show Stray Whitespace](#show-stray-whitespace)
  * [Single Space for Sentence Spacing](#single-space-for-sentence-spacing)
  * [Indentation](#indentation)
  * [Keep Working Directory Tidy](#keep-working-directory-tidy)
  * [Highlight Parentheses](#highlight-parentheses)
  * [Install Packages](#install-packages)
  * [Add Hooks](#add-hooks)
  * [Colorful Parentheses](#colorful-parentheses)
  * [Custom Command and Key Sequence](#custom-command-and-key-sequence)
  * [Emacs Server](#emacs-server)
  * [Emacs Launcher](#emacs-launcher)
* [Opinion References](#opinion-references)
* [Channels](#channels)
* [License](#license)


Who Is This For?
----------------

Are you an absolute beginner to Emacs? Are you so new to Emacs that
you do not even have `~/.emacs` or `~/.emacs.d` on your file system?
Have you come across recommendations to use starter kits like Doom
Emacs, Spacemacs, etc. but then you wondered if you could use vanilla
Emacs and customize it slowly to suit your needs without having to
sacrifice your productivity in the initial days of using Emacs? Do you
also want your Emacs to look sleek from day zero? If you answered
"yes" to most of these questions, then this project is for you.

The [`.emacs`] file in this project provides a quick way to get
started with setting up your Emacs environment. This document explains
how to do so in a step-by-step manner. This document also explains the
content of [`.emacs`] and [`em`] in a line-by-line manner.

Note that many customizations in the Emacs initialization file
available in this project are a result of the author's preferences.
They may or may not match others' preferences. They may or may not
suit your taste and requirements. Wherever applicable, the pros and
cons of each customization and possible alternatives are discussed in
this document. You are encouraged to read the line-by-line explanation
that comes later in this document, understand each customization, and
modify the initialization file to suit your needs.


What About Programming in CL?
-----------------------------

This section is a little note for aspiring Common Lisp (CL)
programmers. If you do not care about Common Lisp programming, ignore
this section.

For those who wish to use Emacs specifically for programming in CL,
please see [Emacs4CL](https://github.com/susam/emacs4cl). Emacs4CL is
similar to this project, however, Emacs4CL focusses primarily on
setting up a CL development environment with Emacs, SBCL, and SLIME,
as quickly as possible. This project, on the other hand, focusses only
on setting up a general purpose editing and programming environment.

Once you have set up your CL development environment using Emacs4CL,
you can then come back to this project and pick more ideas and Emacs
Lisp code to enhance your Emacs setup from being a CL development
environment to being a more general purpose editing and programming
environment.


Features
--------

This project provides a file named [`.emacs`] that offers the
following features:

  - Disable some UI elements to provide a clean and minimal
    look-and-feel.
  - Show current column number in the mode line.
  - Load a dark color theme named Wombat.
  - Customize the color theme to accentuate the cursor, search
    matches, and comments with different shades of orange.
  - Enable and configure Ido mode to find files and switch buffers
    efficiently.
  - Show trailing whitespace at the end of lines clearly.
  - Show trailing newlines at the end of buffer clearly.
  - Show missing newlines at the end of buffer clearly.
  - Use single spacing convention to end sentences.
  - Use spaces, not tabs, for indentation.
  - Configure indentation settings as per popular coding conventions.
  - Highlight matching parentheses.
  - Move auto-save files and backup files to a separate directory to
    keep our working directories tidy.
  - Do not move original files while creating backups.
  - Automatically install configured packages when Emacs starts for
    the first time.
  - Install Markdown mode for convenient editing of Markdown files.
  - Install and configure Paredit for editing S-expressions
    efficiently.
  - Install and configure Rainbow Delimiters to color parentheses by
    their nesting depth level.
  - Provide a minimal example of a user-defined custom command and a
    custom key sequence.
  - Start Emacs server automatically, so that terminal users can use
    Emacs client to edit files with an existing instance of Emacs.

Additionally, this project also provides a convenience command named
[`em`] that is a thin wrapper around the `emacs` and `emacsclient`
commands. It offers the following features:

  - Start a new instance of Emacs when requested.
  - Open files in an existing Emacs server if a server is running
    already.
  - Automatically start a new Emacs server if a server is not running
    already.

All of these features along with every line of code that enables these
features are explained in the sections below.


Get Started
-----------

This section helps you to set up Emfy quickly and see what the end
result looks like. Perform the following steps to get started:

 1. Install Emacs.

    On macOS, enter the following command if you have
    [Homebrew](https://brew.sh):

    ```sh
    brew install --cask emacs
    ```

    On Debian, Ubuntu, or another Debian-based Linux system, enter the
    following command:

    ```
    sudo apt-get install emacs
    ```

    For other environments, visit https://www.gnu.org/software/emacs/
    to see how to install Emacs.

 2. Copy the Emacs initialization file [`.emacs`] provided here to
    your home directory. Here is an example `curl` command that copies
    the initialization file to its traditional location:

    ```sh
    curl -L https://github.com/susam/emfy/raw/main/.emacs >> ~/.emacs
    ```

    Here is another alternative that copies the initialization file to
    a more convenient location:

    ```sh
    mkdir ~/.emacs.d
    curl -L https://github.com/susam/emfy/raw/main/.emacs >> ~/.emacs.d/init.el
    ```

    Yet another popular alternative is to copy the initialization file
    to an XDG-compatible location as follows:

    ```sh
    mkdir -p ~/.config/emacs
    curl -L https://github.com/susam/emfy/raw/main/.emacs >> ~/.config/emacs/init.el
    ```

    Emacs can automatically load the Emacs initialization file from
    any of the paths used above. See section [The Emacs Initialization
    File][emacs-init-doc] of the Emacs manual for more details about
    this. Most users these days prefer one of the last two locations
    because it allows all Emacs configuration to conveniently remain
    in one directory.

    [emacs-init-doc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

 3. Copy the Emacs launcher script [`em`] provided here to some
    directory that belongs to your `PATH` variable. For example, here
    are a few commands that download this script and places it in
    the `/usr/local/bin/` directory:

    ```sh
    curl -L https://github.com/susam/emfy/raw/main/em >> /tmp/em
    sudo mv /tmp/em /usr/local/bin/em
    chmod +x /usr/local/bin/em
    ```

    The usefulness of this launcher script will be explained in the
    section [Emacs Launcher](#emacs-launcher) later.

 4. Start Emacs:

    ```sh
    emacs
    ```

    On macOS, you may receive the following error message in a dialog
    box: '“Emacs.app” can’t be opened because Apple cannot check it for
    malicious software.' To resolve this issue, go to Apple menu >
    System Preferences > Security & Privacy > General and click 'Open
    Anyway'.

    It may take a minute or so for Emacs to start the very first time.
    When it starts the first time with the new Emacs initialization
    file obtained in the previous step, it installs the packages
    specified in it. This is only a one-time activity. The next time you
    start Emacs, it will start instantly. We will see how [`.emacs`]
    takes care of it in the line-by-line guide later.

Now that your environment is setup, read the next section to learn how
to use this environment in more detail.


Step-by-Step Usage
------------------

### Use Emacs

Emacs is a very powerful and extensible editor. It comes with over
10,000 built-in commands. A small section like this can barely scratch
the surface of Emacs. Yet, this section makes a modest attempt at
getting you started with Emacs and then provides more resources to learn
further. Perform the following steps to get started:

 1. Start Emacs:

    ```sh
    emacs
    ```

 2. Within Emacs, enter the following command to open a file, say,
    `hello.txt`:

    ```
    C-x C-f hello.txt RET
    ```

    A new buffer to edit `hello.txt` is created. If a file with that
    name already exists on your file system, then it loads the content
    of the file into the buffer.

    Note that in the Emacs world (and elsewhere too), the
    notation `C-` denotes the <kbd>ctrl</kbd> modifier key. Thus `C-x`
    denotes <kbd>ctrl</kbd> + <kbd>x</kbd>.

    The notation `RET` denotes the <kbd>enter</kbd> or <kbd>return</kbd>
    key.

    Typing consecutive `C-` key sequences can be optimized by pressing
    and holding down the <kbd>ctrl</kbd> key, then typing the other
    keys, and then releasing the <kbd>ctrl</kbd> key. For example, to
    type `C-x C-f`, first press and hold down <kbd>ctrl</kbd>, then
    type <kbd>x</kbd>, then type <kbd>f</kbd>, and then release
    <kbd>ctrl</kbd>. In other words, think of `C-x C-f` as `C-(x f)`.
    This shortcut works for other modifier keys too.

 3. Now type some text into the buffer. Type out at least 3-4 words. We
    will need it for the next two steps.

 4. Move backward by one word with the following key sequence:

    ```
    M-b
    ```

    Remember from the previous section that `M-` denotes the meta
    modifier key. The above command can be typed with
    <kbd>alt</kbd> + <kbd>b</kbd> or <kbd>option</kbd> + <kbd>b</kbd> or
    <kbd>esc</kbd> <kbd>b</kbd>.

    If you face any issue with the <kbd>alt</kbd> key or the
    <kbd>option</kbd> key, read [Emacs Wiki: Meta Key
    Problems](https://www.emacswiki.org/emacs/MetaKeyProblems).

 5. Now move forward by one word with the following key sequence:

    ```
    M-f
    ```

 5. The `C-g` key sequence cancels the current command. This can be used
    when you mistype a command and want to start over or if you type a
    command partially, then change your mind and then you want to cancel
    the partially typed command. Try out these examples:

    ```
    C-x C-f C-g
    ```

    ```
    C-x C-g
    ```

 7. Save the buffer to a file on the file system with this command:

    ```
    C-x C-s
    ```

 8. Quit Emacs:

    ```
    C-x C-c
    ```

Now you know how to start Emacs, open a file, save it, and quit. Improve
your Emacs knowledge further by taking the Emacs tutorial that comes
along with Emacs. In Emacs, type `C-h t` to start the tutorial.

The key bindings to perform various operations like creating file,
saving file, quitting the editor, etc. may look arcane at first, but
repeated usage of the key bindings develops muscle memory soon and after
having used them for a few days, one does not even have to think about
them. The fingers do what the mind wants effortlessly due to muscle
memory.

While you are getting used to the Emacs key bindings, keep this [GNU
Emacs Reference Card][emacs-ref] handy. Also, if you are using it in
GUI mode, then the menu options can be quite helpful. The menu options
contain frequently used operations. The option for each operation also
displays the key bindings that can be used to invoke the same
operation. If you have hidden the menu bar, you can always invoke it
by typing `<f10>`.

[emacs-ref]: https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf


### Use Paredit

Paredit helps in keeping parentheses balanced and also in performing
structured editing of S-expressions in Lisp code. It provides a powerful
set of commands to manipulate S-expressions in various ways. Perform the
following steps to get started with Paredit:

 1. Run Emacs:

    ```sh
    emacs
    ```

 2. Open an Elisp source file:

    ```
    C-x C-f foo.el
    ```

 3. Type the following code only:

    ```elisp
    (defun square (x
    ```

    At this point, Paredit should have inserted the two closing
    parentheses automatically. The code should look like this:

    ```elisp
    (defun square (x))
                    -
    ```

    The cursor should be situated just after the parameter `x`. The
    underbar shows where the cursor should be.

 4. Type the closing parentheses now. Yes, type it even if the closing
    parenthesis is already present. The cursor should now skip over the
    first closing parenthesis like this:

    ```elisp
    (defun square (x))
                     -
    ```

    Of course, there was no need to type the closing parenthesis because
    it was already present but typing it out to skip over it is more
    efficient than then moving over it with movement commands. This is,
    in fact, a very nifty feature of Paredit. We can enter code with the
    same keystrokes as we would without Paredit.

 5. Now type <code>enter</code> to create a new line just before the
    last parenthesis. A newline is inserted like this:

    ```elisp
    (defun square (x)
      )
      -
    ```

 6. Now type only this:

    ```elisp
    (* x x
    ```

    Again, Paredit would have inserted the closing parenthesis
    automatically. The code should look like this now:

    ```elisp
    (defun square (x)
      (* x x))
            -
    ```

There is a lot more to Paredit than this. To learn more, see [The
Animated Guide to Paredit][paredit-ref].

Note: While many Lisp programmers find Paredit very convenient and
powerful while manipulating S-expressions in Lisp code, there are a few
people who do not like Paredit because they find the Paredit behaviour
intrusive. See the [Opinion References](#opinion-references) section for
more discussion on this topic.

[paredit-ref]: http://danmidwood.com/content/2014/11/21/animated-paredit.html


### Execute Emacs Lisp Code

The previous section shows how to write some Elisp code and how
Paredit helps in keeping the parentheses balanced. In this section, we
will see how to execute some Elisp code.

 1. Run Emacs:

    ```sh
    emacs
    ```

 2. Open an Elisp source file:

    ```
    C-x C-f foo.el
    ```

 3. Enter the following code:

    ```elisp
    (defun square (x)
      (* x x))
    ```

 4. With the cursor placed right after the last closing parenthesis,
    type `C-x C-e`. The name of the function defined should appear in
    the echo area at the bottom. This confirms that the function has
    been defined.

 5. Now add the following code to the Elisp source file:

    ```elisp
    (square 5)
    ```

 6. Once again, with the cursor placed right after the last closing
    parenthesis, type `C-x C-e`. The result should appear in the echo
    area at the bottom.


### Use Rainbow Delimiters

There is not much to learn about using Rainbow Delimiters. In the
previous sections, you must have seen that as you type nested
parentheses, each parenthesis is highlighted with a different color.
That is done by Rainbow Delimiters. It colors each parenthesis according
to its nesting depth level.

Note: Not everyone likes Rainbow Delimiters. Some people find
parentheses in multiple colors distracting. See the [Opinion
References](#opinion-references) section for more discussion on this
topic.


Line-by-Line Explanation
------------------------

This section explains the [`.emacs`] file provided here line-by-line.


### Tweak UI Elements

The first few lines in our [`.emacs`] merely tweak the Emacs user
interface. These are of course not essential for using Emacs. However,
many new Emacs users often ask how to customize the user interface to
add a good color scheme and make it look minimal, so this section
indulges a little in customizing the user interface.

Here is a line-by-line explanation of the UI tweaks in [`.emacs`]:

  - Hide the menu bar:

    ```elisp
    (menu-bar-mode 0)
    ```

    When Emacs runs in a GUI window, by default, it starts with a menu
    bar, tool bar, and scroll bar. Many (but not all) users like to
    hide them in order to make the Emacs frame look clean and minimal.
    Note that in Emacs, the term *frame* refers to the GUI window or
    the region of the desktop where Emacs is displayed. In Emacs, the
    term *window* refers to what we usually call split panes these
    days.

    Many users find the menu bar helpful because it helps in
    discovering new features. Even with the menu bar hidden with the
    above line of Emacs Lisp code, the menu  can be accessed
    anytime easily by typing `<f10>`. If you really want the menu bar
    to be visible at all times, remove the above line of Emacs Lisp
    code or just comment it out by inserting a semicolon (i.e., `;`)
    before the opening parentheses.

  - Hide the tool bar and scroll bar:

    ```elisp
    (when (display-graphic-p)
      (tool-bar-mode 0)
      (scroll-bar-mode 0))
    ```

    The `when` expression checks if Emacs is running with graphic
    display before disabling the tool bar and scroll bar. Without the
    `when` expression, we get the following error on Emacs without
    graphic display support: `Symbol's function definition is void:
    tool-bar-mode`. An example of Emacs without graphics support is
    `emacs-nox` on Debian 10.

    Note that this is only an author's preference. You may comment out
    one or more of these lines if you want to retain the tool bar or
    scroll bar.

  - Inhibit the startup screen with the `Welcome to GNU Emacs` message
    from appearing:

    ```elisp
    (setq inhibit-startup-screen t)
    ```

    If you are a beginner to Emacs, you might find the startup screen
    helpful. It contains links to tutorial, manuals, common tasks, etc.
    If you want to retain the startup screen, comment this line out.

  - Show column number in the mode line:

    ```elisp
    (column-number-mode)
    ```

    By default, Emacs shows only the current line number in the mode
    line. For example, by default, Emacs may display something like
    `L4` in the modeline to indicate that the cursor is on the fourth
    line of the buffer. The above Elisp code enables column number
    display in the mode line. With column number enabled, Emacs may
    display something like `(4,0)` to indicate the cursor is at the
    beginning of the fourth line.


### Customize Theme

In this section, we will choose a dark theme for Emacs. If you do not
like dark themes, you might want to stick with the default theme,
choose another theme, or skip this section.

  - Load a beautiful dark color theme known as `wombat`:

    ```elisp
    (load-theme 'wombat)
    ```

    If you want to check the other built-in themes, type `M-x
    customize-themes RET`. A new window with a buffer named `*Custom
    Themes*` appear. In this buffer, select any theme you want to
    test. After you are done testing, you can close this new window
    with `C-x 0`.

    By default the Wombat theme looks like this:

    <a href="https://i.imgur.com/uRBoXCl.png"><img alt="Screenshot of Wombat default theme"
        src="https://i.imgur.com/uRBoXCl.png" width="580"></a>

    In this theme, the cursor, search matches, and comments can often
    be difficult to spot because they are all colored with different
    shades of gray while the background is also gray. In the next few
    points, we will customize this theme a little to make these
    elements easier to spot by coloring them with different shades of
    orange. In the end, our customized Wombat theme would look like
    this:

    <a href="https://i.imgur.com/8GHqcHi.png"><img alt="Screenshot of Wombat custom theme"
        src="https://i.imgur.com/8GHqcHi.png" width="580"></a>

  - Choose a darker shade of gray for the background color to improve
    the contrast of the theme:

    ```elisp
    (set-face-background 'default "#111")
    ```

  - Choose a pale shade of orange for the cursor, so that it is easy
    to spot on the dark gray background:

    ```elisp
    (set-face-background 'cursor "#c96")
    ```

  - Highlight the current search match with a certain shade of orange
    as the background and a very light shade of gray as the
    foreground:

    ```elisp
    (set-face-background 'isearch "#c60")
    (set-face-foreground 'isearch "#eee")
    ```

  - Highlight search matches other than the current one with a darker
    shade of orange as the background and a darker shade of gray as
    the foreground:

    ```elisp
    (set-face-background 'lazy-highlight "#960")
    (set-face-foreground 'lazy-highlight "#ccc")
    ```

  - Use tangerine yellow to colour the comments:

    ```elisp
    (set-face-foreground 'font-lock-comment-face "#fc0")
    ```

Personal note: I see that many recent color themes choose a dim color
for comments in code. Such color themes intend to underemphasize the
comments. I think comments play an important role in code meant to be
read by humans and should be emphasized appropriately. That's why I
have chosen tangerine yellow for comments in the last point above, so
that the comments are easily noticeable.


### Minibuffer Completion

Emacs comes with Ido mode and Fido mode that help with opening files,
switching between buffers, entering commands, etc. quite efficiently.
Ido incrementally searches for file and buffer names as we type out
the names, list the matching names, and automatically completes the
minibuffer with the best match. Fido mode offers much of the same
functionality that Ido mode provides and additionally helps in
automatically completing inputs in all types of minibuffer prompts
(even those not related to files and buffers).

Strictly speaking, it is not necessary to enable Ido mode if we are
going to enable Fido mode but both modes have their areas of strength,
so we are going to enable both to get the best of both modes.

  - Enable Ido mode:

    ```elisp
    (ido-mode 1)
    ```

    Note that the positive argument `1` is necessary. Without it, this
    function call would simply toggle Ido mode on or off everytime it
    executes. Say, we reload the Emacs initialization file with `M-x
    load-file RET ~/.emacs RET`, without the positive argument, this
    function call would end up disabling Ido mode if it was previously
    enabled. The positive argument ensures that Ido mode remains
    enabled even if we reload the Emacs initialization file.

    With Ido mode enabled, automatic completions appear while trying
    to open files or switch between buffers with `C-x C-f` or `C-x b`,
    respectively. For example, merely typing `C-x C-f /et/ost` may
    lead to Ido mode automatically completing the path to
    `/etc/hosts`, i.e., we may type partial substrings of the various
    components of the path or a substring of the buffer name we want
    to search and Ido mode would search the matching names and
    automatically complete the minibuffer input.

  - Enable Ido mode for all buffer/file reading:

    ```elisp
    (ido-everywhere)
    ```

    Without this function call, Ido mode works for `C-x C-f` and `C-x
    b` but not for other minibuffers that may involve entering file or
    buffer names. For example, with this function call removed, if we
    enter `M-x ediff-files RET`, Ido mode does not perform completions
    in the `ediff-files` minibuffer. But with this function call
    present, Ido mode performs completions in the `ediff-files`
    minibuffer too.

  - Enable flexible string matching for Ido mode:

    ```elisp
    (setq ido-enable-flex-matching t)
    ```

    This variable setting allows flexible string matching while
    searching files or buffers. For example, with this setting, if we
    type `C-x C-f /ec/hst`, Ido mode may automatically complete the
    file path to `/etc/hosts`, i.e., Ido mode would automatically
    complete names even if we omit a few characters while typing out
    the name. Without this variable setting, the string we type must
    always be a valid substring (with no characters omitted within the
    substring) of the name we want to search.

  - Perform automatic completions for all kinds of minibuffer inputs:

    ```elisp
    (fido-mode)
    ```

    This mode allows us to perform automatic completions in all kinds
    of minibuffer inputs, not just while finding files and buffers.
    For example, type `M-x wspmod` and Fido mode should immediately
    autocomplete the partial command `wspmod` to `whitespace-mode`.

As explained at the beginning of this section, Ido mode is not really
necessary if we are going to enable Fido mode. But Ido mode has some
nice features for file name completion that Fido mode does not have.
That is why we enable both Ido mode and Fido mode in this section. For
example, Ido mode can search for visited subdirectories for file name
matches which can be quite convenient. Fido mode cannot do this.

To test how Fido mode completes file names, first create a test
directory of files as follows: `mkdir -p /tmp/a/b/; touch
/tmp/a/b/foo.txt; touch /tmp/a/b/bar.txt`. Then comment out the
`(ido-mode 1)` and `(ido-everywhere)` lines in the Emacs
initialization file, save the file, and restart Emacs, so that Ido
mode is disabled and only Fido mode is enabled. Now type `C-x C-f
/tmp/a/b/foo RET`. Then type `C-x C-f /tmp/bar`. Fido mode does not
automatically complete the partial input `/tmp/bar` to
`/tmp/a/b/bar.txt`.

To test how Ido mode can be useful, now uncomment the `(ido-mode 1)`
line in the Emacs initialization file, save the file, and restart
Emacs, so that Ido mode is enabled again. Now type `C-x C-f
/tmp/a/b/foo.txt RET`. Then type `C-x C-f /tmp/bar`. Ido mode
automatically completes the partial input `/tmp/bar` to
`/tmp/a/b/bar.txt`. Once you are done experimenting like this,
remember to uncomment the `(ido-everywhere)` line too in the end.


### Show Stray Whitespace

While writing text files, it can often be useful to quickly spot any
trailing whitespace at the end of lines or unnecessary trailing new
lines at the end of the file.

  - Highlight trailing whitespace at the end of lines:

    ```elisp
    (setq-default show-trailing-whitespace t)
    ```

    With this variable set, any stray trailing whitespace at the end
    of lines is highlighted (usually with a red background) as shown
    in the screenshot below:

    <!-- lorem1 -->
    <a href="https://i.imgur.com/na53DuT.png"><img alt="Whitespace highlighted"
        src="https://i.imgur.com/na53DuT.png" width="580"></a>

    The screenshot above shows one stray trailing space in the second
    line and two trailing spaces in the third line. These trailing
    spaces can be removed with the key sequence `M-x
    delete-trailing-whitespace RET`.

  - Show the end of buffer with a special glyph in the left fringe:

    ```elisp
    (setq-default indicate-empty-lines t)
    ```

    Showing the end of the buffer conspicuously can be helpful to spot
    any unnecessary blank lines at the end of a buffer. A blank line
    is one that does not contain any character except the terminating
    newline itself. Here is a screenshot that demonstrates this
    feature:

    <!-- lorem2 -->
    <a href="https://i.imgur.com/WzystxA.png"><img alt="Non-existent line indicators"
        src="https://i.imgur.com/WzystxA.png" width="580"></a>

    The screenshot shows that there are two blank lines just before
    the end of the buffer. The tiny horizontal dashes on the left
    fringe mark the end of the buffer. Note: This is similar to how
    Vim displays the tilde symbol (`~`) to show the end of the buffer.
    The trailing blank lines at the end of a buffer can be removed
    with the key sequence `M-x delete-trailing-whitespace RET`.

  - Show buffer boundaries in the left fringe:

    ```elisp
    (setq-default indicate-buffer-boundaries 'left)
    ```

    The buffer boundaries can be useful to check if the last line of
    the buffer has a terminating newline or not. If the buffer does
    not contain a terminating newline, then a top-right corner shape
    (`⌝`) appears in the fringe. For example, see this screenshot of a
    file that does not contain a terminating newline:

    <!-- lorem3 -->
    <a href="https://i.imgur.com/M0nPPEg.png"><img alt="File without terminating newline"
        src="https://i.imgur.com/M0nPPEg.png" width="580"></a>

    If there is only one line in the buffer and that line is
    terminated with a newline then a left-bracket (`[`) appears in the
    fringe. If there are multiple lines in the buffer and the last
    line is terminated with a newline then a bottom-left corner shape
    (`⌞`) appears in the fringe. Here is a screenshot of a file that
    contains a terminating newline:

    <!-- lorem4 -->
    <a href="https://i.imgur.com/KUQDpaJ.png"><img alt="File with terminating newline"
        src="https://i.imgur.com/KUQDpaJ.png" width="580"></a>

    To summarize, these shapes (`[` or `⌞`) show where the last
    newline of the buffer exists. The last newline of the buffer
    exists above the lower horizontal bar of these shapes. No newlines
    exist below the lower horizontal bar. It is a good practice to
    terminate text files with a newline. For most types of files,
    Emacs inserts a terminating newline automatically when we save the
    file with `C-x C-s`.


### Single Space for Sentence Spacing

Emacs uses the rather old-fashioned convention of treating a period
followed by double spaces as end of sentence. However, it is more
common these days to end sentences with a period followed by a single
space.

  - Let a period followed by a single space be treated as end of sentence:

    ```elisp
    (setq sentence-end-double-space nil)
    ```

    This little setting has significant consequences while editing and
    moving around text files. We will discuss two such consequences
    now with two tiny experiments:

    **Experiment A: Moving By Sentences:** To check the default
    behaviour, first comment out the above line of Emacs Lisp code in
    the Emacs initialization file, save the file, and restart Emacs.
    Now copy the following text:

    ```
    Lorem ipsum dolor sit amet, consectetur adipiscing elit donec. Porttitor id lacus non consequat.
    ```

    Then open a new text buffer in Emacs with `C-x C-f foo.txt RET`
    and paste the copied text with `C-y`. Then type `C-a` to go to the
    beginning of the line. Finally, type `M-e` to move to the end of
    the sentence. Without `sentence-end-double-space` set to `nil`,
    typing `M-e` moves the cursor all the way to the end of the line
    (i.e., after the second period). It ignores the first period as
    end of sentence because this period is followed by one space
    whereas Emacs expects two spaces after a period at the end of a
    sentence.

    Now to verify that the above line of Emacs Lisp code works as
    expected, uncomment it again to enable it, save the file, restart
    Emacs, and then perform the above experiment again. With
    `sentence-end-double-space` set to `nil`, typing `M-e` moves the
    cursor moves to the end of the of first sentence (i.e., after the
    first period). This is what we normally expect these days.

    **Experiment B: Filling Paragraphs:** While writing text files, it
    is customary to limit the length of each line to a certain maximum
    length. In Emacs, the key sequence `M-q` invokes the
    `fill-paragraph` command that works on the current paragraph and
    reformats it such that each line is as long as possible without
    exceeding 70 characters in length.

    To check the default behaviour, first comment out the above line
    of Emacs Lisp code in the Emacs initialization file, save the
    file, and restart Emacs. Then create the same text buffer as the
    one in the previous experiment. Now place the cursor anywhere on
    text and type `M-q` to reformat it as a paragraph. Without
    `sentence-end-double-space` set to `nil`, typing `M-q` reformats
    the paragraph as follows:

    ```
    Lorem ipsum dolor sit amet, consectetur adipiscing elit
    donec. Porttitor id lacus non consequat.
    ```

    Now to verify that the above line of Emacs Lisp code works as
    expected, uncomment it again to enable it, save the file, then
    restart Emacs, and then perform the above experiment again. With
    `sentence-end-double-space` set to `nil`, typing `M-q` reformats
    the paragrapha as follows:

    ```
    Lorem ipsum dolor sit amet, consectetur adipiscing elit donec.
    Porttitor id lacus non consequat.
    ```

    We see that without `sentence-end-double-space` set to `nil`,
    Emacs refuses to insert a hard linebreak after the string
    `donec.`, so it moves the entire word to the next line. This is a
    result of following the old-fashioned convention of recognizing a
    period followed by double spaces as end of sentence. This
    convention prevents inadvertently placing a hard linebreak within
    an abbreviation. Since it is now more common to end a sentence
    with a single period followed by a single space, we would like the
    text above to be reformatted as shown in the last example above.
    Setting `sentence-end-double-space` to `nil` achieves this.


### Indentation

The following point shows how to configure Emacs to insert spaces, not
tabs, for indenting code.

  - Use spaces, not tabs, for indentation:

    ```elisp
    (setq-default indent-tabs-mode nil)
    ```

    Emacs uses a mix of tabs and spaces by default for indentation and
    alignment. To verify the default behaviour, first comment out the
    above line of Emacs Lisp code, save it, then restart Emacs, then
    open a new Emacs Lisp source file, say, `C-x C-f foo.el RET` and
    type the following three lines of Emacs Lisp code:

    ```elisp
    (defun foo ()
      (concat "foo"
              "bar"))
    ```

    While typing the above code, do not type <kbd>tab</kbd> or
    <kbd>space</kbd> to indent the second and third lines. When you
    type <kbd>enter</kbd> at the end of each line, Emacs automatically
    inserts the necessary tabs and spaces to indent the code. After
    entering this code, type `M-x whitespace-mode RET` to visualize
    whitespace characters. This mode displays each space with a middle
    dot (`·`) and each tab with a right pointing guillemet (`»`). With
    whitespace mode enabled, you should find that the second line of
    code is indented with two spaces but the third line is indented
    with a single tab followed by two spaces. Emacs has a `tab-width`
    variable that is set to `8` by default. For every `tab-width`
    columns of indentation, Emacs inserts a tab to indent the code.
    The third line requires 10 leading spaces for alignment, so Emacs
    inserts one tab character followed by 2 spaces to make the third
    line look aligned. However, this code would look misaligned on
    another editor with a different `tab-width` setting. That's why we
    configure Emacs to use only spaces to indent and align code.

    Now uncomment the function call to set `indent-tabs-mode` to `nil`
    expected, save it, then restart Emacs, and then perform the above
    experiment involving the three lines of Emacs Lisp code again.
    This time, you should see that no tabs are used for indentation.
    Only spaces are used for indentation.

    In some type of files, we must use literal tabs. For example, in
    `Makefile`, the syntax of target rules require that the commands
    under a target are indented by a literal tab character. In such
    files, Emacs is smart enough to always use literal tabs for
    indentation regardless of the above variable setting.

    Mixing tabs and spaces for indenting source code can be
    problematic, especially, when the author of code or Emacs
    inadvertently uses tabs for alignment (as opposed to using tabs
    for indentation only which would be fine) and another programmer
    views the file with an editor with a different tab width setting.
    In fact, in the experiment above, Emacs did use literal tab
    characters to align code which would cause the code to look
    misaligned on another editor with a different tab width setting.
    See [Tabs Are Evil](https://www.emacswiki.org/emacs/TabsAreEvil)
    for more details on this topic.

  - Display the distance between two tab stops as whitespace that is
    as wide as 4 characters:

    ```elisp
    (setq-default tab-width 4)
    ```

    Note that this primarily affects how a literal tab character is
    displayed. Further, along with the previous variable setting where
    we set `indent-tabs-mode` to `nil`, in some types of files, this
    variable setting decides how many spaces are inserted when we hit
    the <kbd>tab</kbd> key. For example, in text buffers, on hitting
    the <kbd>tab</kbd> key, as many spaces are inserted as are
    necessary to move the cursor to the next tab stop where the
    distance between two tab stops is assumed to be `tab-width`.

    In some type of files, we must use literal tabs. For example, in
    `Makefile`, the syntax of target rules require that the commands
    under a target are indented by a literal tab character. In such
    files, Emacs displays the distance between two tab stops as
    whitespace that is as wide as 8 characters by default. This
    default setting is often too large for many users. They feel that
    a tab width of 8 consumes too much horizontal space on the screen.
    The variable setting above reduces the tab width to 4. Of course,
    different users may have different preferences for the tab width.
    Therefore, users are encouraged to modify this variable setting to
    a value they prefer or omit the above line of Emacs Lisp code from
    their Emacs initialization file to leave it to the default value
    of 8.

  - Set indentation levels according to popular coding conventions for
    various languages:

    ```elisp
    (setq c-basic-offset 4)
    (setq js-indent-level 2)
    (setq css-indent-offset 2)
    ```

    Emacs uses 2 spaces for indentation in C by default. We change
    this to 4 spaces.

    Emacs uses 4 spaces for indentation in JavaScript and CSS by
    default. We change this to 2 spaces.


### Highlight Parentheses

The following points describe how we enable highlighting of parentheses:

  - The next point shows how to enable highlighting of matching pair of
    parentheses. By default, there is a small delay between the movement
    of a cursor and the highlighting of the matching pair of
    parentheses. The following line of code gets rid of this delay:

    ```elisp
    (setq show-paren-delay 0)
    ```

    This line of code must come before the one in the next point for it
    to be effective.

  - Highlight matching parentheses:

    ```elisp
    (show-paren-mode)
    ```

    A pair of parenthesis is highlighted when the cursor is on the
    opening parenthesis of the pair or just after the closing
    parenthesis of the pair.


### Keep Working Directory Tidy

Emacs creates a number of temporary files to ensure that we do not
inadvertently lose our work while editing files. However, these files
can clutter our working directories. This section shows some ways to
keep the current working directory tidy by asking Emacs to manage
these files at a different location.

  - Create a directory to keep auto-save files:

    ```elisp
    (make-directory "~/.tmp/emacs/auto-save/" t)
    ```

    In the next point, we discuss auto-save files in detail and ask
    Emacs to write auto-save files to a separate directory instead of
    writing them to our working directory. Before we do that, we need
    to create the directory we will write the auto-save files to,
    otherwise Emacs would fail to write the auto-save files and
    display the following error: `Error (auto-save): Auto-saving
    foo.txt: Opening output file: No such file or directory,
    /Users/susam/.tmp/emacs/auto-save/#!tmp!foo.txt#`

    Note that this issue occurs only for auto-save files, not for
    backup files discussed in the third point of this list. If the
    parent directory for backup files is missing, Emacs creates it
    automatically. However, Emacs does not create the parent directory
    for auto-save files automatically, so we need the above line of
    Emacs Lisp code to create it ourselves.

  - Write auto-save files to a separate directory:

    ```elisp
    (setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
    ```

    If we open a new file or edit an existing file, say, `foo.txt` and
    make some changes that have not been saved yet, Emacs
    automatically creates an auto-save file named `#foo.txt#` in the
    same directory as `foo.txt` every 300 keystrokes, or after 30
    seconds of inactivity. Emacs does this to ensure that the unsaved
    changes are not lost inadvertently. For example, if the system
    crashes suddenly while we are editing a file `foo.txt`, the
    auto-save file would keep a copy of our unsaved worked. The next
    time we try to edit `foo.txt`, Emacs would warn that auto-save
    data already exists and it would then suggest us to recover the
    auto-save data using `M-x recover-this-file RET`. These auto-save
    files are removed automatically after we save our edits but until
    then they clutter our working directories. The above line of Emacs
    Lisp code ensures that all auto-save files are written to a
    separate directory, thus leaving our working directories tidy.

  - Write backup files to a separate directory:

    ```elisp
    (setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
    ```

    If we create a new file or edit an existing file, say, `foo.txt`,
    then make some changes to it, and save it, the previous copy of
    the file is saved as a backup file `foo.txt~`. These backup files
    too clutter our working direcories. The above line of Emacs Lisp
    code ensures that all backup files are written to a separate
    directory, thus leaving our working directories tidy.

  - Create backup files by copying our files, not moving our files:

    ```elisp
    (setq backup-by-copying t)
    ```

    Everytime Emacs has to create a backup file, it moves our file to
    the backup location, then creates a new file at the same location
    as that of the one we are editing, copies our content to this new
    file, and then resumes editing our file. This causes any hard link
    referring to the original file to be now referring to the backup
    file.

    To experience this problem due to the default behaviour, first
    comment out the above line of Emacs Lisp code in the Emacs
    initialization file, save the file, and restart Emacs. Then create
    a new file along with a hard link to it with these commands: `echo
    foo > foo.txt; ln foo.txt bar.txt; ls -li foo.txt bar.txt`. The
    output should show that `foo.txt` and `bar.txt` have the same
    inode number and size because they both refer to the same file.
    Now run `emacs foo.txt` to edit the file, add a line or two to the
    file, and save the file with `C-x C-s`. Now run `ls -li foo.txt
    bar.txt` again. The output should show that `foo.txt` now has a
    new inode number and size while `bar.txt` still has the original
    inode number and size. The file `bar.txt` now refers to the backup
    file instead of referring to the new `foo.txt` file.

    To see the improved behaviour with the above line of Emacs Lisp
    code, uncomment it to enable it again in the Emacs initialization
    file, save the file, restart Emacs and perform the same experiment
    again. After we save the file, we should see that both `foo.txt`
    and `bar.txt` have the same inode number and size.

  - Disable lockfiles:

    ```elisp
    (setq create-lockfiles nil)
    ```

    As soon as we make an edit to a file, say `foo.txt`, Emacs creates
    a lockfile `.#foo.txt`. If we then launch another instance of
    Emacs and try to edit this file, Emacs would refuse to edit the
    file, then warn us that the file is locked by another Emacs
    session, and provide us a few options regarding whether we want to
    steal the lock, proceed with editing anyway, or quit editing it.
    These lockfiles are removed automatically as soon as we save our
    edits but until then they clutter our directories. Unlike
    auto-save files and backup files, there is no way to tell Emacs to
    write these files to a different directory. We can however disable
    lockfile creation with the above line of Emacs Lisp code.

    **Caution:** Note that disabling lockfiles could be risky if you
    are in the habit of launching multiple Emacs instances while
    editing files. With such a habit, it is easy to make the mistake
    of opening the same file in two different Emacs instances and
    inadvertently overwrite changes made via one instance with changes
    made via another instance. The lockfiles are hidden files anyway,
    so they should not bother you in directory listings. If they
    bother you in, say, `git status` output, consider ignoring the
    lockfiles in `.gitignore` instead of disabling them. Having said
    that, it may be okay to disable lockfiles if you are in the habit
    of launching only a single instance of Emacs for the entire
    lifetime of their desktop session and edit all files via that
    single instance. The [Emacs Server](#emacs-server) and [Emacs
    Launcher](#emacs-launcher) sections later discuss techniques about
    how to make this usage style more convenient. If you are willing
    to follow this style of using Emacs, then it may be okay to
    disable lockfiles. To summarize, if you are in doubt, comment out
    or remove the above line of Emacs Lisp code to keep lockfiles
    enabled.


### Install Packages

The following points describe how we automate the installation of
Emacs packages we need:

  - The following code disables TLS 1.3 to work around a known bug in
    GNU Emacs versions 26.1 and 26.2:

    ```elisp
    (when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
    ```

    See https://debbugs.gnu.org/34341 for more details on the bug. This
    code is not required while using GNU Emacs version 26.3 or 27.1 or a
    later version although leaving this code intact should do no harm
    because this code checks whether the Emacs version is less than 26.3
    before applying the workaround.

  - When we install packages using `package-install` (coming up soon
    in a later point), a few customizations are written automatically
    into the Emacs initialization file (`~/.emacs` in our case). This
    has the rather undesirable effect of our carefully handcrafted
    `~/.emacs` being meddled by `package-install`. To be precise, it
    is the `custom` package invoked by `package-install` that intrudes
    into our Emacs initialization file. To prevent that, we ask
    `custom` to write the customizations to a separate file with the
    following code:

    ```elisp
    (setq custom-file (concat user-emacs-directory "custom.el"))
    ```

    Note that this line of code must occur before the
    `package-install` call.

  - Emacs does not load the custom-file automatically, so we add the
    following code to load it:

    ```elisp
    (load custom-file t)
    ```

    It is important to load the custom-file because it may contain
    customizations we have written to it directly or via the customize
    interface (say, using `M-x customize RET`). If we don't load this
    file, then any customizations written to this file will not become
    available in our Emacs environment.

    The boolean argument `t` ensures that no error occurs when the
    custom-file is missing. Without it, when Emacs starts for the
    first time with our initialization file and there is no
    custom-file yet, the following error occurs: `File is missing:
    Cannot open load file, No such file or directory,
    ~/.emacs.d/custom.el`. Setting the second argument to `t` prevents
    this error when Emacs is run with our initialization file for the
    first time.

  - This is necessary for defining the `package-archives` list we will
    use in the next point.

    ```elisp
    (require 'package)
    ```

  - Add Milkypostman's Emacs Lisp Package Archive (MELPA) to the list of
    archives to fetch packages from:

    ```elisp
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    ```

    By default only GNU Emacs Lisp Package Archive (ELPA) is in the list
    of archives to fetch from. The above line adds MELPA too to the
    list. If you are curious to see what the original value of
    `package-archives` was and what it is now due to the above line,
    type `C-h o package-archives RET`.

  - Initialize the package system:

    ```elisp
    (package-initialize)
    ```

    While initializing the package system, this call also initializes
    the `package-archive-contents` variable used in the next point.

  - Download package descriptions from package archives only if they
    have not been downloaded before:

    ```elisp
    (unless package-archive-contents
      (package-refresh-contents))
    ```

    The first line checks whether package descriptions from package
    archives have been fetched. See the `~/.emacs.d/elpa/archives` or
    `~/.config/emacs/elpa/archives` directory for archive contents in
    case you are curious. If the archive contents have not been
    fetched then the second line fetches them. Thus the second line
    executes only when the Emacs initialization is loaded for the
    first time. The first time Emacs starts with the [.emacs](.emacs)
    file of this repository, it takes a while to fetch the package
    archives. However, once the package archives have been fetched and
    Emacs is started again later, it starts instantly because the code
    above takes care not to fetch package archives again when it is
    already cached locally.

  - Install some packages:

    ```elisp
    (dolist (package '(markdown-mode paredit rainbow-delimiters))
      (unless (package-installed-p package)
        (package-install package)))
    ```

    This loops iterates over each package name in a list of packages.
    For each package, it checks whether the package is installed with
    the `package-installed-p` function. If it is not installed, then
    it is installed with the `package-install` function. You can
    modify the list of packages in the first line to add other
    packages that you might need in future or remove packages that you
    do not want.

    The first time Emacs starts with this initialization file, it takes
    a while to install the packages we need. However, once the packages
    are installed and Emacs is started again later, it starts instantly
    because the code above takes care to not attempt installing packages
    that are already installed.


### Add Hooks

This section describes how to enable Paredit and Rainbow Delimiters.
These are not absolutely essential for using Emacs. However many Emacs
Lisp programmers find them useful while some do not.

In case you decide not to use either Paredit or Rainbow Delimiters,
then you may skip this section. In that case, you might also want to
remove these packages from the `package-list` variable discussed in
the previous section.

  - Enable Paredit while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    ```

    Paredit helps in keeping parentheses balanced and in performing
    structured editing of S-expressions. To test that Paredit is
    enabled for editing Emacs Lisp code, open a new Emacs Lisp file,
    say, `foo.el`. Then type `(`. Paredit should automatically insert
    the corresponding `)`.

  - Enable Paredit in eval-expression minibuffer:

    ```elisp
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-:` to bring up the eval-expression minbuffer
    and type `(`. Paredit should automatically insert the corresponding
    `)`.

  - Enable Paredit while interactively evaluating Emacs Lisp expressions
    in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer
    appears, type `(`. Paredit should automatically insert the
    corresponding `)`.

  - Enable Paredit in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    ```

    To test this, first open a non-Lisp file, say, `C-x C-f foo.txt
    RET`. Now type `(`. Note that no corresponding `)` is inserted
    because we are not in Lisp interaction mode yet. Delete `(`. Then
    start Lisp interaction mode with the command `M-x
    lisp-interaction-mode RET`. Type `(` again. Paredit should now
    automatically insert the corresponding `)`.

  - Enable Paredit while editing Lisp code other than Emacs Lisp:

    ```elisp
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    ```

    To test this, open a new Common Lisp source file, say, `C-x C-f
    foo.lisp RET`. Then type `(`. Paredit should automatically insert
    the corresponding `)`.

  - Enable Rainbow Delimiters while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    Rainbow Delimiters color nested parentheses with different colors
    according to the depth level of each parenthesis. To test this
    open a new Emacs Lisp file, say, `foo.el`. Then type `((((`.
    Rainbow Delimiters should color each parenthesis differently.

  - Enable Rainbow Delimiters while interactively evaluating Emacs Lisp
    expressions in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer comes
    up, type `((((`. Rainbow Delimiters should color each parenthesis
    differently.

  - Enable Rainbow Delimiters in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, first open a non-Lisp file, say, `foo.txt`. Now type
    `((((`. Then start Lisp interaction mode with the command `M-x
    lisp-interaction-mode RET`. Rainbow Delimiters should now color each
    parenthesis differently.

  - Enable Rainbow Delimiters while editing Lisp code other than Emacs
    Lisp:

    ```elisp
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, open a new Common Lisp source file, say, `foo.lisp`.
    Then type `((((`. Rainbow Delimiters should color each parenthesis
    differently.

You may have noticed that we did not enable Rainbow Delimiters for
eval-expression. That is because it does not work as expected as of
Dec 2021. See https://github.com/Fanael/rainbow-delimiters/issues/57 for
more details.


### Colorful Parentheses

The default colors that the Rainbow Delimiters package chooses for the
nested parentheses are too subtle to easily recognize the matching
pair of parentheses. Some Lisp programmers like to customize the
colors to make the parentheses look more colorful. This section shows
one way to do this.

  - This is necessary to use the various Rainbow Delimiters faces that
    appear in the next point.

    ```elisp
    (require 'rainbow-delimiters)
    ```

  - Set different colors for parentheses at different nesting level.

    ```elisp
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
    (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
    (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
    (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
    (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
    (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
    (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
    (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
    ```


### Custom Command and Key Sequence

In this section we will see how to make our own custom command.

  - Create a very simple custom command to display the current time in
    the echo area at the bottom of the Emacs frame:

    ```elisp
    (defun show-current-time ()
      "Show current time."
      (interactive)
      (message (current-time-string)))
    ```

    This command can be invoked by typing `M-x show-current-time RET`.
    On running this command, the current time should be displayed in
    the echo area for two seconds.

  - Create a custom key sequence to invoke the command defined in the
    previous point:

    ```elisp
    (global-set-key (kbd "C-c t") 'show-current-time)
    ```

    Now the same command can be invoked by typing `C-c t`.


### Emacs Server

Many users prefer to run a single instance of Emacs and do all their
editing activities via this single instance. It is possible to use
Emacs alone for all file browsing needs and never use the terminal
again. Despite the sophisticated terminal and file browsing
capabilities of Emacs, some users still like to use a traditional
terminal to move around a file system, find files, and edit them. This
practice may become inconvenient quite soon because it would lead to
the creation of too many Emacs frames (desktop-level windows) and
processes. This section explains how to create a single Emacs server,
a single Emacs frame, and edit all your files in this frame via the
server even while you are browsing files in the terminal. You don't
need this section and the next one if you use Emacs for all your file
browsing needs but if you don't, this section may be useful. Let us
now see how we start the Emacs server in our Emacs initialization
file.

  - This is necessary to use the function `server-running-p` coming up
    in the next point:

    ```elisp
    (require 'server)
    ```

    If we omit the above line of Emacs Lisp code, we will encounter
    the following error when we try to use `server-running-p`
    discussed in the next point: `Symbol’s function definition is
    void: server-running-p`.

  - If there is no Emacs server running, start an Emacs server:

    ```elisp
    (unless (server-running-p)
      (server-start))
    ```

    The `unless` expression ensures that there is no Emacs server
    running before starting a new Emacs server. If we omit the
    `unless` expression, the following error would occur if an Emacs
    server is already running: `Warning (server): Unable to start the
    Emacs server. There is an existing Emacs server, named "server".
    To start the server in this Emacs process, stop the existing
    server or call ‘M-x server-force-delete’ to forcibly disconnect
    it.`

    Finally, the `server-start` function call starts an Emacs server.

When Emacs starts for the first time with the above lines of Emacs
Lisp code in its initialization file, it starts an Emacs server. Now
the following commands can be used on a terminal to edit files:

  - `emacs` or `emacs foo.txt bar.txt`: Starts another instance of
    Emacs. It does not start a new server due to the `unless`
    expression discussed above. Typically, we will not use this
    because we don't want to launch a second instance of Emacs. But it
    is good to know that this command still works as expected in case
    we ever need it.

  - `emacsclient foo.txt bar.txt`: Opens files in the existing
    Emacs instance via the Emacs server. The command waits for us to
    finish editing all the files. It blocks the terminal until then.
    When we are done editing a file, we must type `C-x #` to tell
    Emacs to switch to the next file. Once we are done editing all the
    files, the `emacsclient` command exits and the shell prompt
    returns on the terminal.

  - `emacsclient -n foo.txt bar.txt`: Opens files in the existing
    Emacs instance but does not wait for us to finish editing. The
    command exits immediately and the shell prompt returns immediately
    on the terminal.

With this setup, the Emacs server quits automatically when we close
the first Emacs instance that started the Emacs server. Running the
`emacs` command or starting Emacs via another method after that would
start the Emacs server again.

It is worth noting here that there are other ways to start the Emacs
server and to use the `emacsclient` command. See section [Using Emacs
as a Server][emacs-server-doc] and section [`emacsclient`
Options][emacs-client-doc] for more details.

[emacs-server-doc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
[emacs-client-doc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html


### Emacs Launcher

In the previous section, we saw how our Emacs initialization file
ensures that an Emacs server is started when we run `emacs` for the
first time. Once Emacs server has started, we can edit new files from
the terminal using the `emacsclient` command. This section describes a
script named `em` that can automatically decide whether to run `emacs`
or `emacsclient` depending on the situation.

As mentioned in the previous section, you don't need this section if
you use Emacs for all your file browsing needs but if you don't, this
section may be useful. Further, it is worth mentioning that this
script solves a very specific problem of using a single command named
`em` to both launch a new Emacs server as well as to open existing
files in an existing Emacs frame via the existing Emacs server. If you
have this specific problem, you may find this script helpful. However,
if you do not have this problem or if you have a different problem to
solve, it would be useful to understand how `emacsclient` works and
read the related documentation mentioned in the previous section and
then modify this script or write your own shell script, shell alias,
or shell function that solves your problems.

The `em` script should be already present on the system if the steps
in the [Get Started](#get-started) section were followed. The third
step there installs this script to `/usr/local/bin/em`. Now we discuss
every line of this script in detail.

  - When `em` is run without any arguments, start a new Emacs process:

    ```sh
    #!/bin/sh
    if [ "$#" -eq 0 ]
    then
        echo "Starting new Emacs process ..." >&2
        nohup emacs &
    ```

    This Emacs process launches a new Emacs frame. Further, if an
    Emacs server is not running, it starts a new Emacs server. The
    `nohup` command ensures that this Emacs process is not terminated
    when we close the terminal or the shell where we ran the `em`
    command.

  - When `em` is run with one or more filenames as arguments and there
    is an Emacs server already running, edit the files using the
    existing Emacs server:

    ```sh
    elif emacsclient -n "$@" 2> /dev/null
    then
        echo "Opened $@ in Emacs server" >&2
    ```

    However, if there is no Emacs server already running, the above
    command in the `elif` clause fails and the execution moves to the
    `else` clause explained in the next point.

  - When `em` is run with one or more filenames as arguments and there
    is no Emacs server running, start a new Emacs process to edit the
    files:

    ```sh
    else
        echo "Opening $@ in a new Emacs process ..." >&2
        nohup emacs "$@" &
    fi
    ```

    The new Emacs process also starts a new Emacs server.

Now that we know what the `em` script does, let us see the various
ways of using this script as a command:

  - `em`: Start a new instance of Emacs. If there is no Emacs server
    running, this ends up starting an Emacs server too. Normally, this
    command should be run only once after logging into the desktop
    environment.

  - `em foo.txt bar.txt`: Edit files in an existing instance of Emacs
    via Emacs server. If no Emacs server is running, this ends up
    starting an Emacs server automatically. This command is meant to
    be used multiple times as needed for editing files while browsing
    the file system in a terminal.

That's it! Only two things to remember from this section: start Emacs
with `em` and edit files with `em foo.txt`, `em foo.txt bar.txt`, etc.


Opinion References
------------------

- [Give paredit mode a chance][paredit-chance]
- [Never warmed up to paredit][paredit-never-warmed]
- [Coloring each paren differently only adds noise][rainbow-noise]

[paredit-chance]: https://stackoverflow.com/a/5243421/303363
[paredit-never-warmed]: https://lobste.rs/s/vgjknq/emacs_begin_learning_common_lisp#c_0y6zpd
[rainbow-noise]: https://lobste.rs/s/vgjknq/emacs_begin_learning_common_lisp#c_1n78vl


Channels
--------

The following channels are available for asking questions, seeking
help and receiving updates regarding this project:

- GitHub: [emfy/issues](http://github.com/susam/emfy/issues)
- Twitter: [@susam](https://twitter.com/susam)
- Matrix: [#susam:matrix.org](https://matrix.to/#/#susam:matrix.org)
- Libera: [#susam](https://web.libera.chat/#susam)

You are welcome to follow or subscribe to one or more of these channels
to receive updates and ask questions about this project.


License
-------

This is free and open source software. You can use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of it,
under the terms of the MIT License. See [LICENSE.md][L] for details.

This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
express or implied. See [LICENSE.md][L] for details.

[L]: LICENSE.md
