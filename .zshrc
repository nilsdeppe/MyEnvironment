# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# If you want to profile the ZSH startup time to get an idea of
# what is taking up your precious time insert the following
# at the beginning of your .zshrc:
#   zmodload zsh/zprof
# and at the end of your .zshrc add:
# zprof

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

############################################################################
# Set machine to Linux or Mac
UNAME_OUT="$(uname -s)"
case "${UNAME_OUT=}" in
    Linux*)     MACHINE_TYPE=Linux;;
    Darwin*)    MACHINE_TYPE=Mac;;
    CYGWIN*)    MACHINE_TYPE=Cygwin;;
    MINGW*)     MACHINE_TYPE=MinGw;;
    *)          MACHINE_TYPE="UNKNOWN:${unameOut}"
esac

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
#
# On macOS (probably Linux too) you can install PL10k via:
# git clone --depth=1 https://github.com/romkatv/powerlevel10k.git \
#     ~/.oh-my-zsh/custom/themes/powerlevel10k
if [ -f $ZSH/custom/themes/powerlevel10k/powerlevel10k.zsh-theme ]; then
    # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
    # Initialization code that may require console input (password prompts, [y/n]
    # confirmations, etc.) must go above this block; everything else may go below.
    if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
        source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
    fi

    ZSH_THEME="powerlevel10k/powerlevel10k"
    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
elif [ -f $ZSH/custom/themes/agnoster-mine.zsh-theme ]; then
    ZSH_THEME="agnoster-mine"
else
    ZSH_THEME=robbyrussell
fi

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Clone fast-syntax-highlighting
if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fast-syntax-highlighting ]; then
    git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git \
        ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fast-syntax-highlighting
fi

# Clone zsh-autosuggestions
if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions ]; then
    git clone https://github.com/zsh-users/zsh-autosuggestions \
        ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
fi

# Clone zsh-completions
if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions ]; then
    git clone https://github.com/zsh-users/zsh-completions \
        ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
fi

# Which plugins would you like to load? (plugins can be found in
# ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    command-not-found
    docker
    emoji
    extract
    gem
    gpg-agent # Need to setup GPG first. On macOS this means install via brew
    git
    gitfast
    git-extras
    history
    mosh
    per-directory-history
    pep8
    pip
    pipenv
    pylint
    python
    ripgrep
    rust
    ubuntu
    wd
    web-search
    zsh-autosuggestions
    zsh-completions
    # fast-syntax-highlighting must be at the end!
    fast-syntax-highlighting
)

# Support bash-completions
autoload -U compinit && compinit

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Since we use a multiline prompt, make it so that we don't clutter the backlog
# with the extra prompt info such as the path and version control info.
clean_past_prompt() {
    # Borrowed from:
    # https://www.reddit.com/r/zsh/comments/j06vmp/
    #         multiline_prompts_without_pollution/

    # Clear last 2 lines (command and extra line with path+version control info)
    printf "\033[2A\033[2K"
    # Do a bunch of regex stuff...
    0="${1//\\/\\\\\\\\}"
    0="${0//\\n/\\\\n}"
    0="${0//\$/\\\\$}"
    0="${0//\`/\\\\\`}"
    0="${0//\%/%%}"
    # Print a blank line followed by the command entered
    print -P -- "$0"
    print -P -- "$0"
}

# if [[ "$TERM" == (alacritty*|gnome*|konsole*|putty*|rxvt*|screen*|tmux*|xterm*) ]]; then
#     add-zsh-hook -Uz preexec clean_past_prompt
# fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
if [ -f $HOME/.ssh/rsa_id ]; then
    export SSH_KEY_PATH="~/.ssh/rsa_id"
fi

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

############################################################################
# Set default user for powerline agnoster theme
DEFAULT_USER=`whoami`

############################################################################
# Color man pages
function man() {
    env LESS_TERMCAP_mb=$(printf "\e[1;31m") \
  LESS_TERMCAP_md=$(printf "\e[1;34m") \
  LESS_TERMCAP_me=$(printf "\e[0m") \
  LESS_TERMCAP_se=$(printf "\e[0m") \
  LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
  LESS_TERMCAP_ue=$(printf "\e[0m") \
  LESS_TERMCAP_us=$(printf "\e[4;1;32m") \
  man "$@"
}

#############################################################################
# Enable the zcalc calculator
autoload -Uz zcalc

#############################################################################
# History changes
# Default is 10,000, prefer 100,000
HISTSIZE=100000
SAVEHIST=100000

# Write the history file in the ":start:elapsed;command" format.
setopt EXTENDED_HISTORY
# Write to the history file immediately, not when the shell exits.
setopt INC_APPEND_HISTORY
# Share history between all sessions.
setopt SHARE_HISTORY
# Expire duplicate entries first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST
# Don't record an entry that was just recorded again.
setopt HIST_IGNORE_DUPS
# Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_ALL_DUPS
# Do not display a line previously found.
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
# Don't write duplicate entries in the history file.
setopt HIST_SAVE_NO_DUPS
# Remove superfluous blanks before recording entry.
setopt HIST_REDUCE_BLANKS
# Don't execute immediately upon history expansion.
setopt HIST_VERIFY
# Beep when accessing nonexistent history.
setopt HIST_BEEP

#############################################################################
# Check if we are running inside a container
in_container() {
    if [[ $IN_CONTAINER == "true" ]] || [ $SPECTRE_CONTAINER ]; then
        return 0
    fi
    return 1
}

############################################################################
# Function to replace a string everywhere in the git repo
git_replace_all() {
    if [ "$#" -ne 2 ]; then
        echo "Illegal number of parameters. Expected two parameters."
        echo "1: string to replace and 2: string to replace with."
    else
        echo "Replacing '$1' with '$2' in git repo."
        if [[ "$MACHINE_TYPE" == "Mac" ]]; then
            git grep -l "$1" | xargs sed -i '' -e "s%$1%$2%g"
        elif [[ "$MACHINE_TYPE" == "Linux" ]]; then
            git grep -l "$1" | xargs sed -i "s%$1%$2%g"
        else
            echo "Unknown machine for git_replace_all: $MACHINE_TYPE"
        fi
    fi
}

# Function to replace a string in each file in directory recursively
replace_all() {
    if [ "$#" -ne 2 ]; then
        echo "Illegal number of parameters. Expected two parameters."
        echo "1: string to replace and 2: string to replace with."
    else
        echo "Replacing '$1' with '$2' in all files in directory and all subdirectories."
        if [[ "$MACHINE_TYPE" == "Mac" ]]; then
            grep -rl "$1" ./ | xargs sed -i '' -e "s/$1/$2/g"
        elif [[ "$MACHINE_TYPE" == "Linux" ]]; then
            grep -rl "$1" | xargs sed -i "s%$1%$2%g"
        else
            echo "Unknown machine for replace_all: $MACHINE_TYPE"
        fi
    fi
}

############################################################################
# function that creates a tarball of the files needed for a paper.
#
# Extracts the includegraphics statements
create_paper_tarball() {
    if [ "$#" -lt 1 ]; then
        echo "Illegal number of parameters. Expected one or more parameters."
        echo "1: paper prefix, eg 'paper' if 'paper.tex'"
        echo "additional parameters are passed to the tar command."
    else
        if [[ "$MACHINE_TYPE" == "Mac" ]]; then
            grep "includegraphics" ./$1.tex | \
                sed -E 's/.*\{(.*)\}.*/\1*/g' | \
                xargs -n 1 sh -c 'echo $0' | \
                xargs tar czf $1.tar.gz $1.tex $1.bbl ${@:2}
        elif [[ "$MACHINE_TYPE" == "Linux" ]]; then
            grep "includegraphics" ./$1.tex | \
                sed -r 's/.*\{(.*)\}.*/\1*/g' | \
                xargs -n 1 sh -c 'echo $0' | \
                xargs tar czf $1.tar.gz $1.tex $1.bbl ${@:2}
        else
            echo "Unknown machine for replace_all: $MACHINE_TYPE"
        fi
    fi
}

############################################################################
# Python
if [ -f ~/.pythonstartup ]; then
    export PYTHONSTARTUP=~/.pythonstartup
fi
# Update all pip packages
if command -v pip > /dev/null 2>&1 && \
       command -v grep > /dev/null 2>&1 && \
       command -v cut > /dev/null 2>&1 && \
       command -v xargs > /dev/null 2>&1; then
    alias pip_upgrade_all="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"
fi

#############################################################################
# git
# Make emacs default git editor
if command -v emacsclient > /dev/null 2>&1; then
    export EDITOR=emacsclient
fi

#############################################################################
# Aliases
if command -v emacsclient > /dev/null 2>&1 &&\
       command -v emacs > /dev/null 2>&1; then
    # Emacs client alias to open in Terminal and gui
    EMACS_LOCATION=`which emacs`
    alias ec="emacsclient -nw -c -a $EMACS_LOCATION"
    alias ecgui="emacsclient -c -a $EMACS_LOCATION"
fi

if command -v tmux > /dev/null 2>&1; then
    # tmux session save and restore
    # https://github.com/mislav/dotfiles/blob/d2af5900fce38238d1202aa43e7332b20add6205/bin/tmux-session
    alias tmux-session="~/Dropbox/UsefulStuff/tmux-session.sh"
fi

#############################################################################
# Enable GDB to break when the address sanitizer finds an issue
export ASAN_OPTIONS=abort_on_error=1

#############################################################################
# Set Ninja status par
if command -v ninja > /dev/null 2>&1; then
    export NINJA_STATUS="[%r:%f/%t-%e]"
    # Shortcut to clear screen and run ninja
    alias cninja="clear && clear && ninja"

    # Set up ninja completion
    COMP_DIR=${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/
    if [ ! -f ${COMP_DIR}/ninja-completion  ]; then
        wget https://raw.githubusercontent.com/ninja-build/ninja/master/misc/zsh-completion \
             -O ${COMP_DIR}/ninja-completion
    fi
    fpath=(${COMP_DIR}/_ninja $fpath)
fi

#############################################################################
# Do operations we only want to do inside containers
if in_container; then
    # Source the module system
    if [ -f /etc/profile.d/lmod.sh ]; then
        . /etc/profile.d/lmod.sh
    fi
fi

#############################################################################
# Do operations we only want to do outside the containers
if ! in_container; then
    #############################################################################
    # Spack
    # Load LMod for modules support. Where lmod is can depend on your system
    if [ -f /usr/share/lmod/lmod/init/bash ]; then
        . /usr/share/lmod/lmod/init/bash
    fi

    SPACK_DIR=$HOME/Research/spack/bin
    if [ -d $SPACK_DIR ]; then
        # Add Spack bin path
        export PATH=$PATH:$HOME/Research/spack/bin
        # Soruce Spack env
        . $HOME/Research/spack/share/spack/setup-env.sh

        #############################################################################
        # Load useful modules
        load_modules() {
            # module load \
            #        brigand-master-gcc-9.1.0-gazgu4v \
            #        xsimd-7.2.3-gcc-9.1.0-bqasdid
        }
        load_modules
    fi

    LIBSHARP_DIR=$HOME/Research/libsharp
    if [ -d $LIBSHARP_DIR ]; then
        # Set up libsharp
        export PATH=$LIBSHARP_DIR/auto/bin:$PATH
        export CPATH=$LIBSHARP_DIR/auto/include:$CPATH
        export LD_LIBRARY_PATH=$LIBSHARP_DIR/auto/lib:$LD_LIBRARY_PATH
        export LIBRARY_PATH=$LIBSHARP_DIR/auto/lib:$LIBRARY_PATH
        export CMAKE_PREFIX_PATH=$LIBSHARP_DIR/auto/:$CMAKE_PREFIX_PATH
    fi

    INTEL_DIR=/opt/intel
    if [ -d $INTEL_DIR ]; then
        #############################################################################
        # Load intel compiler
        export PATH=$PATH:$INTEL_DIR/bin
        if [ -d $INTEL_DIR/vtune_amplifier_2019.6.0.602217/bin64 ]; then
            export PATH=$PATH:$INTEL_DIR/vtune_amplifier_2019.6.0.602217/bin64
        fi
        if [ -d $INTEL_DIR/advisor_2019.5.0.602217/bin64 ]; then
            export PATH=$PATH:$INTEL_DIR/advisor_2019.5.0.602217/bin64
        fi
        if [ -d $INTEL_DIR/inspector_2019.5.0.602217/bin64 ]; then
            export PATH=$PATH:$INTEL_DIR/inspector_2019.5.0.602217/bin64
        fi
    fi

    if command -v ruby > /dev/null 2>&1; then
        #############################################################################
        # Gems for Ruby (used for Jekyll)
        export PATH=$(ruby -e 'print Gem.user_dir')/bin:$PATH
    fi

    # Drop into a Singularity dev environment, reset modules after
    container() {
        if [ "$1" = "spectre" ] && \
               [ -f $HOME/Research/spectre.sif ]; then
            module purge
            singularity shell $HOME/Research/spectre.sif
            load_modules
        elif [ "$1" = "spec" ] && \
             [ -f $HOME/Research/singularity/spec.img ]; then
            module purge
            singularity shell \
                        $HOME/Research/singularity/spec.img
            load_modules
        elif [ "$1" = "arch" ] && \
             [ -f $HOME/Research/singularity/arch.simg ]; then
            module purge
            singularity exec \
                        $HOME/Research/singularity/arch.simg \
                        /bin/zsh
            load_modules
        elif [ "$1" = "ubuntu" ] && \
                 [ -f $HOME/Research/singularity/ubuntu.simg ]; then
            module purge
            singularity exec \
                        $HOME/Research/singularity/ubuntu.simg \
                        /bin/zsh
            load_modules
        else
            echo "No matching container found. Received $@"
        fi
    }
fi

#############################################################################
# Use custom mosh because the upstream doesn't allow forwarding
if [ -d $HOME/bin ]; then
    export PATH=$PATH:$HOME/bin
fi
if command -v $HOME/bin/sga-env.sh > /dev/null 2>&1; then
    . sga-env.sh
    alias mosh-tesla="mosh -p 60001 tesla"
fi

#############################################################################
# Just use command line emacs. I installed a command line emacs
if command -v systemctl > /dev/null 2>&1 \
       && command -v emacs > /dev/null 2>&1; then
    alias restart-emacs="systemctl --user restart emacs.service"
fi

#############################################################################
# When building Emacs lsp-mode enable plists, which are an optimization.
#
# Must also have (defvar lsp-use-plists t) in Emacs config.
export LSP_USE_PLISTS=true

#############################################################################
# Just use command line emacs. I installed a command line emacs
if command -v swapoff > /dev/null 2>&1 \
       && command -v swapon > /dev/null 2>&1; then
    alias clear-swap="sudo swapoff -a && sudo swapon -a"
fi

#############################################################################
# Command to restart WiFi driver on MacBooks
if command -v rmmod > /dev/null 2>&1 \
       && command -v emacs > /dev/null 2>&1; then
    alias restart-wifi="sudo rmmod brcmfmac && sudo modprobe brcmfmac"
fi

#############################################################################
# Alias to the gui firewall
if command -v gufw > /dev/null 2>&1; then
    alias guiufwx="xhost si:localuser:root && sudo gufw"
fi

#############################################################################
# GPG key
if [[ "$MACHINE_TYPE" == "Linux" ]]; then
    export GPG_TTY=$(tty)
fi

#############################################################################
# enable using "fuck" to correct last command
if command -v thefuck > /dev/null 2>&1; then
    eval $(thefuck --alias)
fi

#############################################################################
# Dolphin 5 doesn't work with Wayland yet.
if command -v dolphin-emu > /dev/null 2>&1; then
    alias dolphin-emu="GDK_BACKEND=x11 dolphin-emu"
fi

#############################################################################
# Load Rust installation
RUST_HOME=$HOME/.cargo/bin
if [ -d "$RUST_HOME" ]; then
    export PATH="$RUST_HOME:$PATH"
fi

if [ -d /usr/local/go/bin ]; then
    export PATH=/usr/local/go/bin:$PATH
fi

#############################################################################
# Easily mount and unmount SSHFS volumes
if command -v sshfs > /dev/null 2>&1; then
    mount-sshfs() {
        if [ "$#" -eq 1 ] || [ "$#" -eq 2 ]; then
            DIR_TO_MOUNT=~/$1
            if [ "$#" -eq 2 ]; then
                DIR_TO_MOUNT=$2
            fi
            echo "Mounting home directory of $1 to $DIR_TO_MOUNT"
            if [ ! -d $DIR_TO_MOUNT ]; then
                mkdir $DIR_TO_MOUNT
            fi
            sshfs -o idmap=user $1:$HOME $DIR_TO_MOUNT
        else
            echo "Usage: mount-sshfs SSH_HOSTNAME [DIR_NAME]"
            echo "where the SSH_HOSTNAME matches one specified in ~/.ssh/config"
            echo "the option DIR_NAME is where the SSHFS will be mounted"
            return 1
        fi
    }
    umount-sshfs() {
        if [ "$#" -eq 1 ] || [ "$#" -eq 2 ]; then
            DIR_TO_MOUNT=~/$1
            if [ "$#" -eq 2 ]; then
                DIR_TO_MOUNT=$2
            fi
            echo "Unmounting home directory of $1 from $DIR_TO_MOUNT"
            fusermount -u $DIR_TO_MOUNT
        else
            echo "Usage: unmount-sshfs SSH_HOSTNAME [DIR_NAME]"
            echo "where the SSH_HOSTNAME matches one specified in ~/.ssh/config"
            echo "the option DIR_NAME is where the SSHFS is currently mounted"
            return 1
        fi
    }
fi

#############################################################################
# Set homebrew path on macOS
if [[ "$MACHINE_TYPE" == "Mac" ]]; then
    if [ -d /opt/homebrew/opt/ruby/bin ]; then
        export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
    fi
fi

#############################################################################
# On macOS homebrew's services don't seem to get the PATHs right, so launch
# an Emacs daemon manually.
if [[ "$MACHINE_TYPE" == "Mac" ]]; then
    EMACS_DAEMON_RUNNING=`ps ux | grep -v 'grep' | grep 'emacs --daemon' | wc -l`
    if [ $EMACS_DAEMON_RUNNING -eq 0 ]; then
        emacs --daemon > /dev/null 2>&1
    fi
fi

#############################################################################
# Force SSH agent socket location. Can use systemd service at
# ~/.config/systemd/user/ssh-agent.service
#
#    [Unit]
#    Description=SSH key agent
#
#    [Service]
#    Type=forking
#    Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
#    ExecStart=/usr/bin/ssh-agent -a $SSH_AUTH_SOCK
#
#    [Install]
#    WantedBy=default.target
#
# Run:
#    systemctl --user enable ssh-agent
#    systemctl --user start ssh-agent
#
# Make sure ~/.ssh/config has:
#    AddKeysToAgent  yes
if [ -e "$XDG_RUNTIME_DIR/ssh-agent.socket" ]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

#############################################################################
# To set display for ParaView output when doing SSH use:
#   export DISPLAY=:1

#############################################################################
# Add .local bin since python now puts things there
if [ ! -d ${HOME}/.local/bin ]; then
    export PATH=$PATH:${HOME}/.local/bin
fi

#############################################################################
# Set performance metrics
function enable_perf() {
    echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
    echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
    echo always | sudo tee /sys/kernel/mm/transparent_hugepage/enabled
}

if [[ "$MACHINE_TYPE" == "Mac" ]]; then
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    __conda_setup="$('/Users/nils/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/Users/nils/opt/anaconda3/etc/profile.d/conda.sh" ]; then
            . "/Users/nils/opt/anaconda3/etc/profile.d/conda.sh"
        elif [ -d /Users/nils/opt/anaconda3/bin ]; then
            export PATH="/Users/nils/opt/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
    # <<< conda initialize <<<
fi
