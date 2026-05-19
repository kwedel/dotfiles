# Tools
* atuin
* bat
* tldr
* fzf
* uv
  * httpie
  * llm
  * files-to-prompt
  * nbdime
  * ruff
  * mypy
  * ddo
  * ro
  * unifind
  * dockling
  * markitdown


# gitconfig
<details>

``` ini
[user]
	email = 
	name = 

[init]
	defaultBranch = main

[alias]
    lg = lg1-specific
    lg1 = lg1-specific --all
    lg2 = lg2-specific --all
    lg3 = lg3-specific --all

    lg1-specific = log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)'
    lg2-specific = log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(auto)%d%C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)'
    lg3-specific = log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset) %C(bold cyan)(committed: %cD)%C(reset) %C(auto)%d%C(reset)%n''          %C(white)%s%C(reset)%n''          %C(dim white)- %an <%ae> %C(reset) %C(dim white)(committer: %cn <%ce>)%C(reset)'

    # Show all branches
    branches = !"git for-each-ref --color --sort=-committerdate --format='%(color:yellow)%(refname:short)%(color:reset) | %(color:green)%(committerdate:iso)%(color:reset) %(subject) %(color:dim white)- %(committername)%(color:reset)' refs/heads refs/remotes | column -t -s'|'"

    # Show recently edited files
    recentchanges = !"git ls-files -z . | xargs -0 -I{} git log -1 --pretty=format:'%ct|{}:|%an|%ar|%h %n' -- {} | sort -nr | cut -d'|' -f2- | column -t -s'|'"

    # Open repo in browser  
    browse = "!f() { \
    	  '/mnt/c/Program Files/Internet Explorer/iexplore.exe' `git remote -v \
          | awk '/fetch/{print $2}' \
          | sed -Ee 's#(git@|git://)#https://#' -e 's@com:@com/@'` \
          | head -n1; \
     }; f"

[protocol "file"]
	allow = never

[includeIf "gitdir:/home/kaare/personal/**"]
    path = /home/kaare/.gitconfig-personal

[pager]
	diff = less +/diff\\ --git +g
	show = less '+/^diff --git.*$' +g
```
</details>
