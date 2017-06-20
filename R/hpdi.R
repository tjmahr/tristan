compute_hpdi <- function(xs, prob = .9) {
  x_sorted <- sort(xs)
  n <- length(xs)

  num_to_keep <- ceiling(prob * n)
  num_to_drop <- n - num_to_keep

  possible_starts <- seq(1, num_to_drop + 1, by = 1)
  # Just count down from the other end
  possible_ends <- rev(seq(from = n, length = num_to_drop + 1, by = -1))

  # Find smallest interval
  span <- x_sorted[possible_ends] - x_sorted[possible_starts]
  edge <- which.min(span)
  edges <- c(possible_starts[edge], possible_ends[edge])

  # My requirement: length of span interval must be same as number to keep.
  # Other methods produce intervals that are 1 longer.
  stopifnot(length(edges[1]:edges[2]) == num_to_keep)

  x_sorted[edges]
}
