#!/bin/bash
##########xcc########################################################################################################
# Author	:	Sean Averhoff
# Website	:	https://www.sean-averhoff.com
##################################################################################################################
#
#   DO NOT JUST RUN THIS. EXAMINE AND JUDGE. RUN AT YOUR OWN RISK.
#
##################################################################################################################
echo
echo "################################################################## "
echo "Phase 1 : "
echo "Link .config files to home "
echo "################################################################## "
  mkdir -p ~/.dotfiles/.local/suckless-scripts
  mkdir ~/.dotfiles/.local/wallpapers
  mkdir ~/.dotfiles/.config

  mv ~/.config/arco-dwm ~/.config/arco-dwm-old 
  rm -rf ~/.dotfiles/.config/arco-dwm
  cp -r ~/Dotfiles/.config/arco-dwm ~/.dotfiles/.config/arco-dwm
  ln -sf ~/.dotfiles/.config/arco-dwm ~/.config/arco-dwm

  mv ~/.config/arco-slstatus ~/.config/arco-slstaus-old 
  rm -rf ~/.dotfiles/.config/arco-slstatus
  cp -r ~/Dotfiles/.config/arco-slstatus ~/.dotfiles/.config/arco-slstatus
  ln -sf ~/.dotfiles/.config/arco-slstatus ~/.config/arco-slstatus
  
  rm -rf ~/.config/dmenu 
  cp -r ~/Dotfiles/.config/dmenu ~/.dotfiles/.config/dmenu
  ln -sf ~/.dotfiles/.config/dmenu ~/.config/dmenu

  rm -rf ~/.dotfiles/.config/emacs 
  cp -r ~/Dotfiles/.config/emacs ~/.dotfiles/.config/emacs
  ln -sf ~/.dotfiles/.config/emacs ~/.config/emacs
  rm -rf ~/.emacs.d
  git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
  ln -sf ~/.dotfiles/.config/emacs/.emacs-profiles.el ~/.emacs-profiles.el
  
  rm -rf ~/.config/st 
  cp -r ~/Dotfiles/.config/st ~/.dotfiles/.config
  ln -sf ~/.dotfiles/.config/st ~/.config
  
  rm -rf ~/.config/shell
  cp -r ~/Dotfiles/.config/shell ~/.dotfiles/.config
  ln -sf ~/.dotfiles/.config/shell ~/.config
  ln -sf ~/.config/shell/profile ~/.zprofile
    
  rm -rf ~/.config/zsh 
  cp -r ~/Dotfiles/.config/zsh ~/.dotfiles/.config/zsh
  ln -sf ~/.dotfiles/.config/zsh ~/.config

  #rm -rf ~/.bashrc
  #cp -r ~/Dotfiles/.bashrc ~/.bashrc
  
##################################################################################################################
echo
echo "################################################################## "
echo "Phase 2 : "
echo "Link .local files to home & create wallpaper link"
echo "################################################################## "
  cp -r ~/Dotfiles/.local/suckless-scripts ~/.dotfiles/.local
  ln -sf ~/.dotfiles/.local/suckless-scripts ~/.local
  
  cp -r ~/Dotfiles/.local/wallpapers ~/.dotfiles/.local
  ln -sf ~/.dotfiles/.local/wallpapers ~/.local
  
  ln -sf ~/.dotfiles/.local/wallpapers/deepin.jpg ~/.dotfiles/.local/wallpapers/bg

##################################################################################################################
echo
echo "################################################################## "
echo "Phase 3 : "
echo "Make scripts executable"
echo "################################################################## "
sudo chmod +x ~/.dotfiles/.local/suckless-scripts

##################################################################################################################
echo
echo "################################################################## "
echo "Phase 4 : "
echo "Install Repositorys"
echo "################################################################## "
pacman -S xwallpaper neovim
