dotfiles
=====================================
by Todd Gamblin, tgamblin@llnl.gov

This repo contains tools to help you keep your home directory
synchronized across machines.

Overview
-------------------------------------
To get started there are three easy steps:

1. Fork this repo, then clone it somewhere in your home directory.
2. Put your dotfiles in `home` and check them in.
3. Run the `link` script to create symbolic links in your home directory
   to everything in `home`.

Now your dotfiles are in a git repo and you can clone them anywhere and keep them synchronized.

Setup
-------------------------------------
Fork this repo and put your dotfiles in it:

    git clone git@github.com:your-name/dotfiles.git .dotfiles
    cp .bashrc .bash_profile .emacs .gitconfig .dotfiles/home

Then commit them all to the git repo to keep them synced.

Linking and Unlinking
-------------------------------------
Setting your environment up on a new machine is now simple:
    
    git clone git@github.com:your-name/dotfiles.git .dotfiles
    .dotfiles/link

If something goes wrong, not to worry.  `link` keeps backups in `~/.dotfiles-backup`.  You can run `unlink` to delete all the symbolic links and put yourold config files back where they were:

    .dotfiles/unlink

Run .dotfiles/help for more information.
