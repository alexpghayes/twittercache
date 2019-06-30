
<!-- README.md is generated from README.Rmd. Please edit that file -->

# twittercache

<!-- badges: start -->

<!-- badges: end -->

`twittercache` facilitates robust sampling of the Twitter network. The
basic idea is to save any data into a local cache as you as you get it.
`twittercache` is build on top of [`rtweet`](http://rtweet.info/).

## Installation

You can install the development version of `twittercache` with:

``` r
install.packages("devtools")
devtools::install_github("alexpghayes/twittercache")
```

`twittercache` is not on CRAN and I don’t plan to submit it.

## Basic workflow

1.  Register your tokens with `register_token()`.
2.  Add users to the request queue with `request()`.
3.  Actually get the friendship information with
    `sample_twitter_graph()`.
4.  Export the graph with `get_network_as_tidygraph()`.

## Step 1: Register your tokens

Create a token with `rtweet` and then register it with
`register_token()`:

``` r
library(twittercache)

token <- rtweet::create_token(
  app = "FILL_THIS_IN",
  consumer_key = "FILL_THIS_IN",
  consumer_secret = "FILL_THIS_IN",
  access_token = "FILL_THIS_IN",
  access_secret = "FILL_THIS_IN"
)

register_token(token)
```

`twittercache` is smart and only allows you to register a token once, so
don’t worry about accidentally registering the same token multiple
times. You can check how many tokens you have registered with

``` r
get_number_of_tokens()
```

## Step 2: Request information on nodes in the twitter graph

Request information on Twitter users with `request()`:

``` r
request("alexpghayes")
```

You can pass in multiple screen names at once, and you can also request
information on all the 1-step neighors of these users by setting
`neighborhood = TRUE`.

``` r
rohe_lab <- c("alexpghayes", "karlrohe", "krauskae", "_fchen",
              "JosephineLukito", "yinizhang2011")

request(rohe_lab, neighborhood = TRUE)
```

`request()` *does not* do any sampling. It just builds a queue of nodes
that need to be sampled. `request()` is smart:

  - If a node is already in the cache, it isn’t added to the queue
  - Redundant nodes are not added to the queue
  - Requests for users that don’t exist gives a warning but otherwise
    carries on happily

To see the current list of requests, use

``` r
get_requests()
```

See the reference section for more details about how to manage the
request list.

## Step 3: Sample nodes\!

All you need to do is run

``` r
sample_twitter_graph()
```

Really. That’s it. Data about each user is saved into the cache as soon
it is received.

If your R session crashes, you lose internet connection, whatever, it
doesn’t matter. Just run `sample_twitter_graph()` again and the sampling
will pick up right where it left off, automatically updating the request
queue for you.

Sampling will automatically use all registered tokens, and will respect
rates limits. When we reach an API rate limit, we simply wait for the
shortest amount of time until the rate limit resets for the next
available token.

On occasion `rtweet()` or the Twitter API will just explode and won’t be
able to sample a node. `sample_twitter_graph()` records these failures
and moves on to the next requested node in cases like this. See
`get_failures()` and associated tools for some details on how to handle
this.

## Step 4: Turn the data into a graph\!

All you need to do to turn your hard won data into a graph is call:

``` r
graph <- get_network_as_tidygraph()
graph
```

This can take a hot second. I recommend saving the resulting object as
an `.rds` to facilitate easy use down the line.

Please see the
[`tidygraph`](https://www.data-imaginist.com/2017/introducing-tidygraph/)
blog post for details on how to use `tidygraph` objects. In particular,
you’ll probably want to do some sort of subsetting of nodes and edges.

A useful trick to know is that you can get the adjacency matrix with:

``` r
igraph::get.adjacency(graph)
```

## Frequently asked questions

**Where is the cache itself?**

It’s at `~/.twittergraph/`.

**This whole cache thing is a bad idea**

Probably, but it works for me.

**What about users with big follower counts? Do you sample all the
followers?**

No. We only sample up to 5,000 friends and 5,000 followers. This is
mostly because I’m lazy and also it’s wasteful to spend API calls on
someone with tons of followers.

Recall that you can pick up an edge in the network from either node. We
recommend trying to make sure the node with smaller degree is in your
sample.

**What happens when I try to sample protected users?**

We remove these users from the sample at export time.

**What is this mess of weird logger calls?**

If someone knows better practices for logging things, please reach out,
I really have no clue what I’m doing here.

## Contributing

Please note that the ‘twittercache’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
