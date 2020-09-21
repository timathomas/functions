# List of error solutions

## Problems

### `warning: unable to access '/Users/timothythomas/.config/git/attributes': Permission denied`
solution: 
	* [url](https://stackoverflow.com/questions/27150926/unable-to-access-git-attributes)
	* file is root and not user

	cd ~/
	ls -al
	<Noticed .config was owned by root, unlike everything else in $HOME>
	sudo chown -R $(whoami) .config

