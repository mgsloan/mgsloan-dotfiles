See the comments in [99-batify.rules.mustache][] and
[90-backlight.rules][] for a description of what these udev rules do.

To generate `99-batify.rules`, run `./generate.sh`, which requires
`mustache`. `mustache` can typically be installed via `sudo gem
install mustache`.

To apply these rules, run `sudo ./apply.sh`.
