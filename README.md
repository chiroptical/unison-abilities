# Lightning Talk

Compares free monad, tagless final, and unison abilities for a `Stack` effect

Get the latest PDF: https://github.com/chiroptical/unison-abilities/releases/latest/download/unison-abilities.pdf

##### Notes

- To make a new release,

```
tag=v0.1a0
git tag -a $tag -m "Release $tag"
git push origin $tag
# Update default.nix to point to the new release when it builds
nix-build # build the project, need to update sha256
nix-store --query --references $(nix-instantiate shell.nix) \
  | xargs nix-store --realise \
  | xargs nix-store --query --requisites \
  | cachix push chiroptical # cache the shell environment
git commit -am "Release $tag"
git push
```
