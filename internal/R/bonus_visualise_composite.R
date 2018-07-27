visualisation_composite_indicator_definition_graph<-function(composite_indicators_definitions_weighted_counts){


composite_indicators_definitions_weighted_counts$value_label<-composite_indicators_definitions_weighted_counts %>%
  split.data.frame(composite_indicators_definitions_weighted_counts$var) %>%
  lapply(function(x){
    question_get_choice_labels(gsub(","," ",x$value),x$var[1])}) %>% unlist

composite_indicators_definitions_weighted_counts$var_label<-question_get_question_label(composite_indicators_definitions_weighted_counts$var)

# condition node names:
composite_indicators_definitions_weighted_counts$condition_node<-paste0(
  composite_indicators_definitions_weighted_counts$var," - ",
  composite_indicators_definitions_weighted_counts$condition, " ",
  composite_indicators_definitions_weighted_counts$value,
  ": ",
  composite_indicators_definitions_weighted_counts$weight)

# condition label:
composite_indicators_definitions_weighted_counts$condition_label<-paste0(
  composite_indicators_definitions_weighted_counts$condition, " ",
  composite_indicators_definitions_weighted_counts$value," ->",composite_indicators_definitions_weighted_counts$weight)

# make edgelist:
source<-composite_indicators_definitions_weighted_counts$var
target<-composite_indicators_definitions_weighted_counts$new.var.name
condition<-composite_indicators_definitions_weighted_counts$condition_node
el<-rbind(cbind(source,condition),cbind(condition,target))

# make nodelist:
condition_vertices<-cbind(name=condition,
                          type="condition",
                          label=composite_indicators_definitions_weighted_counts$condition_label)
condition_vertices<-condition_vertices[!duplicated(condition_vertices[,"name"]),]

composite_vertices<-cbind(name=unique(target),
                          type="composite",
                          label=unique(target))
variable_vertices<-cbind(name=unique(source),type="variable",
                         label=question_get_question_label(unique(source)))

variable_vertices<-variable_vertices[!(variable_vertices[,"name"] %in% composite_vertices[,"name"]),]

vertex_list<-rbind(variable_vertices,condition_vertices,composite_vertices)


g<-graph_from_data_frame(el,directed = T,vertices = vertex_list)
V(g)$type[degree(g,mode = "out")==0]<-"composite_final"




vertex_color_table<-data.frame(

  type=c("variable",
         "condition",
         "composite",
         "composite_final"),

  color=c(reach_style_color_darkgrey(2),
          reach_style_color_beige(2),
          reach_style_color_red(2),
          reach_style_color_red(1)
          )
  )

V(g)$color<-as.character(vertex_color_table$color[match(V(g)$type,vertex_color_table$type)])


g_independent_components<-decompose.graph(g)


lapply(seq_along(g_independent_components),function(gi){
  g<-g_independent_components[[gi]]
  area_sidelength_per_20_nodes<-10
  plotwidth<-sqrt((area_sidelength_per_20_nodes^2)*length(V(g))/20)
  
  
  pdf(file = paste0("./output/composite_indicator_visualisation/",
                    "composite_indicator_graph_",gi,".pdf"),
      plotwidth,plotwidth)
  plotwidthpx<-sqrt((area_sidelength_per_20_nodes^2)*length(V(g))/20)*150
  jpeg(file = paste0("./output/composite_indicator_visualisation/",
                     "composite_indicator_graph_",gi,".jpg")
       ,plotwidthpx,plotwidthpx)
  
  plot_composite_graph(g)
  dev.off()
  dev.off()
  
#   save_graph_to_d3_web(g,
#                        path = ".",
#                        paste0(file="./output/composite_indicator_visualisation/composite_indicator_graph_",gi,".html"))
#   
})

}










graph_reverse_edges<-function(g){
graph_as_dfs<-as_data_frame(g,"both")
old_from<-graph_as_dfs$edges$from
graph_as_dfs$edges$from<-graph_as_dfs$edges$to
graph_as_dfs$edges$to<-old_from
graph_from_data_frame(graph_as_dfs$edges,directed = T,vertices = graph_as_dfs$vertices)
}








plot_composite_graph<-function(g){
  
  # plot parameters
  # size based on pagerank:
  pagerank<-page.rank(graph_reverse_edges(g),directed = T,damping = 1)$vector[names(V(g))]
  pagerank<-(pagerank-min(pagerank))
  pagerank<-pagerank/max(pagerank)
  size<-pagerank
  neutral="black"
  
  # plot size
  plot(g,
       vertex.color=V(g)$color,
       vertex.frame.color=NA,
       vertex.size=(size+1)*5,
       vertex.label.color=neutral,
       edge.color="#CCCCCC",
       edge.width=2,edge.curved=F,vertex.label.dist=0)
  
  
  
  }



save_graph_to_d3_web<-function(g, path, filename){
  
  gd3<-igraph_to_networkD3(g,group = V(g)$type)
  fn<-forceNetwork(Links = gd3$links,Nodes = gd3$nodes,Source="source",
                   Target="target",NodeID = "name",Group="group",fontSize = 14,
                   legend = T,zoom = F,bounded = T,arrows = F,opacity = 1,opacityNoHover = 1,charge = -100)
  oldwd<-getwd()
  setwd(path)
  networkD3::saveNetwork(fn, filename, selfcontained = TRUE)
  setwd(oldwd)
 }


