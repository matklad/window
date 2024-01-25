# Window --- constant-time live `grep`

I can't grep, so I wrote this tool to help me out with the following problem:

I have a large (10s of gigabytes) log of a deterministic simulation of a distributed system. The log
captures some failure (cluster fails to converge or some such nastiness). Among the million of
messages which capture _everything_ that happened to the cluster, I want to quickly identify the
small subset that explains the failure.

In _theory_, this is solvable on the command line using the right combination of `grep`, `tail`,
`head` and such, but I am not comfortable with coreutils to actually be able to use these tools
efficiently.

Enter `window`:

[Screencast.webm](https://github.com/matklad/window/assets/1711539/11b81248-253c-4844-ae4a-3146f64d950f)

On the left, there's a file with a filtering query. On the right --- the result of applying the
filter to the entire log file. Changing the query automatically updates results.

Secret sauce: `window` looks only at the constant amout of bytes from the input, and the size of the
output is also bound by a constant:

```toml
source_bytes_max = 104857600
target_bytes_max = 102400
```

That means:

- Its on the user to positon this 100MiB "window" above the interesting part of the log file
- In exchange, processing is constant time --- a query with low specificity returns an empty result
  quickly, instead of grinding the computer to a halt.

## Usage Instrutions

See the source code: at the moment, this is a tool for my personal use, so I haven't invested any
time into making it user friendly, beyond writing this readme.
