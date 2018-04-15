# XSVim [![Gitter](https://badges.gitter.im/XSVim/Lobby.svg)](https://gitter.im/XSVim/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/nosami/XSVim.svg?branch=7.4)](https://travis-ci.org/nosami/XSVim)


# Installation

Interact with Visual Studio for Mac as follows:

```
Visual Studio -> Extensions -> Gallery -> IDE Extensions -> "VIM" -> Install
```

Then close the current document that you are working on and open a new document to activate the plugin.

# What works?

Most Vim commands should work. If you see something that doesn't work, please file an issue. There's a good chance that I just don't know about it.

# What doesn't work

- Vim split windows. XSVim uses VS for Mac's side by side mode to emulate this, but it's only possible to have 2 vertical split windows. `<C-w>s` and `<C-w>v` both switch to side by side mode.
- Visual block mode works for most tasks, but there are some differences in the way that VS handles virtual spacing at the end of lines.
- Selecting text with the mouse or using cmd+arrow keys doesn't switch to Visual mode
- `.` for repeat breaks when the commands to be repeated contain intellisense insertions.
- No leader key support or configurable key bindings.

# Extras

- `gd` - Goto declaration
- `gu` - Find usages
- `gb` - Go to base symbol
- `gh` - Show tooltip at current caret location (`G`o `H`over)
- `hjkl` support on the Solution Explorer pad and Test Explorer pad. Pressing `<esc>` on these will switch focus back to the last editor window. This feature does not yet work on the Search Results pad.

# Looking for the latest release?

Check the [release page](https://github.com/nosami/XSVim/releases) as there is usually a more recent version of the addin here than on the Visual Studio for Mac feed. Grab the .mpack file and install it via Visual Studio -> Extensions -> Install from file

# Support & Contributions

Jump in our [Gitter channel](https://gitter.im/XSVim/Lobby) and introduce yourself. 

# With thanks to

- @shirshov
- @mdizzy
- @tdfacer

