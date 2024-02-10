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


## Design Notes

`window` combines insights from rust-analyzer and TigerBeetle:

* `target_bytes_max` comes from rust-analyzer --- the end consumer of any tool's output is a human.
  Humans are notoriously slow and are incapable of processing more than a hundred lines at a time.
  So, any processing should be hard-capped to produce at most a screenfull of output. If that's not
  enough, the correct solution is for the user to refine the query, rather than for the computer to
  burry the human in a needle-laced haystack.

* `source_bytes_max` comes from `TigerBeetle` --- although default programming model is to treat all
  resources as virtually unlimited (the only enforced limit is the 64-bit size of the address
  space), any real software running on a real hardware will have _some_ limits on the size of the
  dataset it can process. By _starting_ from the explicit limits, the software can become
  qualitatively more reliable, and usually much snappier as well!

See <https://matklad.github.io/2024/02/10/window-live-constant-time-grep.html> for an expanded
discussion.
