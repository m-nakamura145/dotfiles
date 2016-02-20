#-----------
# 環境変数
#-----------
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export EDITOR=/usr/local/Cellar/emacs/24.3/bin/emacs
export PAGER=/usr/bin/less
export MANPAGER=/usr/bin/less
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/Cellar/:$PATH
export PATH=$HOME/.rbenv/shims:$PATH
export PATH=$HOME/.rbenv/bin:$PATH
export PATH=$HOME/.nodebrew/current/bin:$PATH
export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin
export PATH=$HOME/.nodebrew/current/bin:$PATH

#-----------
# プロンプト
#-----------
# 色を有効にする
autoload -Uz colors; colors

# emacs 風キーバインドにする
bindkey -e

# ディレクトリ名を入力するだけでcdできるようにする
setopt auto_cd

# 入力しているコマンド名が間違っている場合にもしかして：を出す。
setopt correct

# ビープを鳴らさない
setopt nobeep

# 変数展開
setopt prompt_subst

# ^Dでログアウトしない。
setopt ignoreeof

# バックグラウンドジョブが終了したらすぐに知らせる。
setopt no_tify

# 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups

# コマンドのスペルチェック
setopt correct

# ヒストリに時刻情報もつける
setopt extended_history

# 空白ではじまるコマンドをヒストリに保持しない
setopt hist_ignore_space

# 重複するヒストリを持たない
setopt HIST_IGNORE_ALL_DUPS

# 関数定義をヒストリに入れない
setopt HIST_NO_FUNCTIONS

# history コマンドをヒストリに入れない
setopt HIST_NO_STORE

# 履歴から冗長な空白を除く
setopt HIST_REDUCE_BLANKS

# 改行コードで終らない出力もちゃんと出力する
setopt no_promptcr

# 複数プロセスで履歴を共有
setopt SHARE_HISTORY

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# '#' 以降をコメントとして扱う
setopt interactive_comments

# = の後はパス名として補完する
setopt magic_equal_subst

# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu

# 高機能なワイルドカード展開を使用する
setopt extended_glob

# zsh のバージョンチェック
autoload -Uz is-at-least

# 左プロンプト表示形式変更
PROMPT='[%F{cyan}%B%n%b%f@%F{cyan}%U%m%u%f]# '

#-----------
# 補完
#-----------
# 補完機能の強化
#For zsh-completions
fpath=(/usr/local/share/zsh-completions $fpath)
autoload -U compinit; compinit -u

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

#-----------
# vcs_info
#-----------
# VCSの情報を取得するzshの便利関数 vcs_infoを使う
autoload -Uz vcs_info

# 表示フォーマットの指定
# %b ブランチ情報
# %a アクション名(mergeなど)
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

# バージョン管理されているディレクトリにいれば表示，そうでなければ非表示
# カレントディレクトリ表示
RPROMPT="[%F{green}%~%f]%1(v|%F{green}%1v%f|)"

# 最新の行のみ右プロンプト表示
setopt transient_rprompt

#-----------
# エイリアス
#-----------
#alias emacs="emacs -nw"
alias emacs="/usr/local/Cellar/emacs/24.3/bin/emacs"
alias grep="grep --color -n -I --exclude='*.svn-*' --exclude='entries' --exclude='*/cache/*'"

# ls
alias ls="ls -G" # color for darwin
alias l="ls -la"
alias la="ls -la"
alias ll="ls -1"

# tree
alias tree="tree -NC" # N: 文字化け対策, C:色をつける

alias be='bundle exec'
alias gg='git gr'
alias gs='git status'
alias ctags="`brew --prefix`/bin/ctags"

#-----------
# その他
#-----------
# cdしたあとで、自動的に ls する
function chpwd() { ls -1 }

# peco
function peco-select-history() {
    typeset tac
    if which tac > /dev/null; then
        tac=tac
    else
        tac='tail -r'
    fi
    BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle redisplay
}
zle -N peco-select-history
bindkey '^r' peco-select-history

function peco-src () {
    local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src

function peco-pkill() {
    for pid in `ps aux | peco | awk '{ print $2 }'`
    do
        kill $pid
        echo "Killed ${pid}"
    done
}
alias pk="peco-pkill"

alias -g B='`git branch | peco | sed -e "s/^\*[ ]*//g"`'

# hub
function git(){hub "$@"}

alias see='git browse'
