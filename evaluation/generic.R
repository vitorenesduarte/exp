x_axis_label <- function(text) {
  mtext(
    side=1, # bottom
    text=text,
    font=2, # bold
    line=.8, # closeness to plot
    outer=TRUE, # outside of the plot if TRUE
    cex=1 # size
  )
}

y_axis_label <- function(text) {
  mtext(
    side=2, # left
    las=0, # vertical text
    text=text,
    font=2, # bold
    line=.8, # closeness to plot
    outer=TRUE, # outside of the plot if TRUE
    cex=1 # size
  )
}

add_title <- function(text) {
  title(text, cex.main=1.3, line=0.7)
}

plot_bars <- function(title, lines, y_min, colors, angles, densities, bar_cex=1) {
  # find the y max
  y_max = Reduce(max, lines)
  y_max = y_max + 0.15*y_max

  # configure and draw
  p <- barplot(
    lines,
    ylim=c(y_min, y_max),
    xpd = FALSE,
    col=colors,
    horiz=FALSE,
    angle=angles,
    density=densities
  )

  # round tops
  rounded <- map(
    lines,
    function(v) {
      x <- round(v, digits=1)
      if(x == 1) round(v, digits=2) else x
    }
  )
  
  # add text at top of bars
  text(
    x=p,
    y=lines,
    label=rounded,
    pos=3,
    cex=bar_cex,
    font=2
  )

  # title
  add_title(title)
}

plot_cdf <- function(title, lines, colors, y_max, y_step) {
  lines_y <- seq(0, y_max, by=y_step)
  lines_x <- lapply(
    lines,
    function(l) { quantile(l, probs=lines_y, names=FALSE) }
  )

  # find x min and max
  x_min <- Reduce(min, lapply(lines_x, min))
  x_max <- Reduce(max, lapply(lines_x, max))

  # configure plot
  plot(
    range(x_max),
    range(1),
    xlim=c(x_min, x_max),
    ylim=c(0, y_max),
    xlab="",
    ylab="",
  )

  # draw
  for(i in 1:length(lines_x)) {
    lines(
      lines_x[[i]],
      lines_y,
      col=colors[[i]],
      lty=i,
      lwd=i,
    )
  }

  # title
  add_title(title)
}

plot_lines <- function(title, lines_x, lines_y, colors) {
  # find the x max and y max
  x_max <- Reduce(max, lapply(lines_x, max))
  y_max <- Reduce(max, lapply(lines_y, max))

  # configure plot
  plot(
    range(x_max),
    range(y_max),
    type="n",
    xlim=c(0, x_max), # max x
    ylim=c(0, y_max), # max y
    xlab="",
    ylab="",
  )

  # draw
  for(i in 1:length(lines_y)) {
    lines(
      lines_x[[i]],
      lines_y[[i]],
      col=colors[[i]],
      type="b",
      pch=i
    )
  }

  # title
  add_title(title)
}

plot_box <- function(title, lines, colors) {
  boxplot(
    lines,
    notch=TRUE,
    log="y"
  )

  # title
  title(title, cex.main=1.5, line=0.7)
}
