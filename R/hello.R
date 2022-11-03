# network stuff -----------------------------------------------------------

#' @title convert an edge list into 2 data frames: nodes and edges
#' @description This function takes a data.frame like with two or three columns:
#' `FROM` and `TO` (and potentially `VALUE`) and outputs a list containing two
#' dataframes: Nodes and Edges. The simplest way to accomplish this task would be to
#' create an edge list using the igraph function `from_edgelist()`, and then
#' subsequently we could use `as_data_frame()` to create each data.frame.
#' This function provides some additional functionality for convenience for the
#' extremely lazy, suitable for subsequent manipulation
#'
#' @param df dataframe with edgeList
#' @param Index Should the IDs of nodes start from 0 or 1. The indexing is important.
#' By default, the edges will be 1-indexed (for VisNetwork), but if you're using
#' network3d, change the indexing to 0, Default: 1
#' @param addLabel should the node names be displayed?
#' @param addHover should the node names be displayed on hover?
#' @param col1 what color should from nodes be?, Default: #A58FAA
#' @param col2 what color should from nodes be?, Default: #A6D6D6
#' @param printResults View node degrees to help w/ trim limits?
#' @return output will be a list containing both dataframes
#' @details nothin
#' @importFrom dplyr pull mutate %>% count bind_rows ungroup slice rename
#' @examples
#' \dontrun{
#' if(interactive()){
#' g <-
#'  #EXAMPLE1
#'  }
#' }
#' @rdname edgeListToNodesEdges
#' @export

edgeListToNodesEdges <- function(df,addLabel = TRUE, addHover = FALSE,
                                 col1 = "#A58FAA", col2 = "#A6D6D6", Index=1,
                                 printResults = F){
  nodes <- data.frame(name=df[,1:2] %>% unlist %>% as.character() %>% unique())
  nodes[,1] <- as.character(nodes[,1])
  nodes$id <- 1:nrow(nodes)

  ## and match to IDs to make edges
  edges <- data.frame(from= match(df[,1], nodes$name),
                      to=   match(df[,2], nodes$name),
                      stringsAsFactors = FALSE)
  if (ncol(df) == 3) edges <- edges %>% mutate(value = pull(df,3))

  ## indexing
  if (Index==0){
    edges$from <- as.integer(edges$from - 1)
    edges$to <- as.integer(edges$to - 1)
  }

  if (addLabel) nodes$label <- nodes$name
  if (addHover) nodes$title <- nodes$name

  ## and add node sizes and colors:
  nodeSizeColors <- bind_rows(
    df %>% select(1) %>% set_names("name") %>%
      count(name, name = "value") %>% mutate(color = col1),
    df %>% select(2) %>% set_names("name") %>%
      count(name, name = "value") %>% mutate(color = col2)
  )

  nodes <- nodes %>% left_join(nodeSizeColors, by = "name")

  if (printResults) {
    par(mfrow=c(1,2))
    nodes$value %>% sort %>% plot(main = "look for 'big' nodes")
    grid()
    nodes$value %>% hist(main="look for 'small' nodes")
  }

  if (any(duplicated(nodes$id)))
    warning("Nodes found in FROM and TO... node colors/sizes might not be representative")
  nodes <- nodes %>% group_by(id) %>% slice(1) %>% ungroup
  edges <- edges %>% count(from,to) %>%
    rename(value = n) %>%
    mutate(arrows = "to")

  return(list(nodes = nodes, edges = edges))
}

#' @title filter a network according to simple node counts
#' @description This is a dangerous thing to do if you're interested in statistics,
#' but for visualization purposes, it can be useful to make some decisions about what's
#' being removed. For the purposes of this function, the count is the node's degrees.
#' @param object The list that contains nodes and edges, (the output of createNetwork)
#' @param eliminateAbove nodes larger than this number will be omitted, Default: NULL
#' @param eliminateBelow nodes smaller than this number will be omitted, Default: NULL
#' @param printResults View node degrees to help w/ trim limits?
#' @importFrom dplyr %>% filter pull count
#' @return a list with 3 objects, same structure as the input
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname truncateNodes
#' @export

truncateNodes <- function(object, eliminateAbove = NULL, eliminateBelow = NULL,
                          printResults = F){
  # browser()
  ## maybe filter away the most populous ones:

  nodes <- object$nodes
  edges <- object$edges

  if (!is.null(eliminateAbove)) {  ## maybe take off big nodes
    toRemove <- nodes %>% filter(value > eliminateAbove) %>% pull(id)
    nodes <- nodes %>% filter(!id %in% toRemove)
  }

  if (!is.null(eliminateBelow)) {  ## maybe take off little nodes
    toRemoveS <- nodes %>% filter(value < eliminateBelow) %>% pull(id)
    nodes <- nodes %>% filter(!id %in% toRemoveS)
  }

  edges <- edges %>% filter(from %in% nodes$id,to %in% nodes$id)

  ## this in turn obviates some nodes... so remove nodes w/out edges again.
  nodes <- nodes %>% filter(id %in% (c(edges$from, edges$to) %>% unique))

  nodeCounts <- tibble(id = c(edges$from, edges$to))
  nodeCounts <- nodeCounts %>% count(id,name = "value")

  if (printResults) {
    print(nodeCounts %>% arrange(desc(value)) %>% head(10) %>%
            left_join(nodes %>% select(id, name)))

    par(mfrow=c(1,2))
    nodeCounts$value %>% sort %>% plot(main = "look for high n")
    grid()
    nodeCounts$value %>% hist(main="look for ones")
  }

  return(list(nodes = nodes, edges = edges, nodeCounts = nodeCounts))
}


## Network plotter
#' @title Create a prettier igraph chart
#' @description FUNCTION_DESCRIPTION
#' @param g graph from igraph
#' @param size What should govern node sizes ('D' for Degree, , 'B' for Betweenness,
#' 'A' for Authority, 'H' for Hub, 'C' for Eigenvector Centrality), Default: 'D'
#' @param color What should govern the node colors? Same options as size (D,B,A,H),
#' Default: NULL
#' @param layout What layout should be used ('FR' for Fruchterman Reingold, 'G' for
#' Graphopt, 'K' for Kamada Kawai), Default: 'F'
#' @param nodeScale how much should we exaggerate the scale for node sizes, Default: 2
#' @param as Arrow head size, Default: 0.1
#' @param keepBackbone What percent of edges should be shown? We are using the
#' `layout_as_backbone` function from `graphlayouts` to trim less meaningful edges
#' for aesthetic purposes, Default = 0.5
#' @importFrom graphlayouts layout_as_backbone
#' @importFrom igraph hub_score layout.fruchterman.reingold layout.graphopt
#' layout.kamada.kawai authority.score degree betweenness
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'
#'  }
#' }
#' @rdname igraphPlot
#' @export

igraphPlot <- function(g, size = "D", color = NULL, layout = "FR",
                       nodeScale = 2, as = 0.1, keepBackbone = 0.5){
  g <- simplify(g)
  #V(g)$grp <- as.character(rep(1:9,each=20))
  bb <- graphlayouts::layout_as_backbone(g,keep=keepBackbone)
  g <- subgraph.edges(g, eids = bb$backbone, delete.vertices = F)

  if (size == "H") vSize <- nodeScale * hub_score(g)$vector
  if (size == "A") vSize <- nodeScale * authority.score(g)$vector
  if (size == "D") vSize <- nodeScale * degree(g)
  if (size == "B") vSize <- nodeScale * igraph::betweenness(g)$vector

  if (layout == "FR") lay <- layout.fruchterman.reingold
  if (layout == "G") lay <- layout.graphopt
  if (layout == "K") lay <- layout.kamada.kawai

  plot(g,
       main = 'TITLE',
       vertex.color = rainbow(52),
       vertex.size = vSize,
       edge.arrow.size = as,
       layout=lay
  )
}
