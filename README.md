# Dotfiles (Patrick Flor)

My minimal Linux dotfiles.


## How to install

```bash
$ bash -c "$(curl -fsSL raw.github.com/senorflor/linux-dotfiles/master/bin/dotfiles)"
```

N.B. If you wish to fork this project and maintain your own dotfiles, you must
substitute my username for your own in the above command and the 2 variables
found at the top of the `bin/dotfiles` script. If there are
permissions problems with the dotfiles linking, try the same command,
replacing "master" with "sudo-ln":

```bash
$ bash -c "$(curl -fsSL raw.github.com/senorflor/linux-dotfiles/sudo-ln/bin/dotfiles)"
```


## How to update

You should run the update when:

* You make a change to `~/.dotfiles/git/gitconfig` (the only file that is
  copied rather than symlinked).
* You want to pull changes from the remote repository.

Run the dotfiles command:

```bash
$ dotfiles
```

Options:

<table>
    <tr>
        <td><code>-h</code>, <code>--help</code></td>
        <td>Help</td>
    </tr>
</table>


## Features

### Custom bash prompt

I use a custom bash prompt based on the Solarized color palette and influenced
by @gf3's and @cowboy's custom prompts. For best results, you should install
iTerm2 and import [Solarized
Dark.itermcolors](https://github.com/altercation/solarized/tree/master/iterm2-colors-solarized).

When your current working directory is a Git repository, the prompt will
display the checked-out branch's name (and failing that, the commit SHA that
HEAD is pointing to). The state of the working tree is reflected in the
following way:

<table>
    <tr>
        <td><code>+</code></td>
        <td>Uncommitted changes in the index</td>
    </tr>
    <tr>
        <td><code>!</code></td>
        <td>Unstaged changes</td>
    </tr>
    <tr>
        <td><code>?</code></td>
        <td>Untracked files</td>
    </tr>
    <tr>
        <td><code>$</code></td>
        <td>Stashed files</td>
    </tr>
</table>

Further details are in the `bash_prompt` file.

Screenshot:

![](http://i.imgur.com/DSJ1G.png)

### Local and private configurations

Any special-case Vim directives local to a machine should be stored in a
`.vimrc.local` file on that machine. The directives will then be automatically
imported into your master `.vimrc`.

Any private and custom commands should be stored in a `~/.bash_profile.local`
file. Any commands included in this file will not be under version control or
committed to a public repository. If `~/.bash_profile.local` exists, it will be
sourced for inclusion in `bash_profile`.

Here is an example `~/.bash_profile.local`:

```bash
# PATH exports
PATH=$PATH:~/.gem/ruby/1.8/bin
export PATH

# Git credentials
# Not under version control to prevent people from
# accidentally committing with your details
GIT_AUTHOR_NAME="Patrick Flor"
GIT_AUTHOR_EMAIL="florp@example.com"
GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
# Set the credentials (modifies ~/.gitconfig)
git config --global user.name "$GIT_AUTHOR_NAME"
git config --global user.email "$GIT_AUTHOR_EMAIL"
```

The `git/gitconfig` file is copied to `~/.gitconfig`, so any private git
configuration specified in `~/.bash_profile.local` will not be committed to
your dotfiles repository.

## Acknowledgements

Inspiration and code for @necolas's dotfiles, which I have neutered in
order to make this repo, was taken from many sources, including:

* [@mathiasbynens](https://github.com/mathiasbynens) (Mathias Bynens)
  [https://github.com/mathiasbynens/dotfiles](https://github.com/mathiasbynens/dotfiles)
* [@tejr](https://github.com/tejr) (Tom Ryder)
  [https://github.com/tejr/dotfiles](https://github.com/tejr/dotfiles)
* [@gf3](https://github.com/gf3) (Gianni Chiappetta)
  [https://github.com/gf3/dotfiles](https://github.com/gf3/dotfiles)
* [@cowboy](https://github.com/cowboy) (Ben Alman)
  [https://github.com/cowboy/dotfiles](https://github.com/cowboy/dotfiles)
* [@alrra](https://github.com/alrra) (Cãtãlin Mariş)
  [https://github.com/alrra/dotfiles](https://github.com/alrra/dotfiles)
