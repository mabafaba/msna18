visualisation_composite_indicator_definition_graph<-function(composite_indicators_definitions_weighted_counts){

require("igraph")


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
                          label=composite_indicators_definitions_weighted_counts$condition_label,
                          color=reach_style_color_red(3))
condition_vertices<-condition_vertices[!duplicated(condition_vertices[,"name"]),]
composite_vertice<-cbind(name=unique(target),type="composite",
                         label=unique(target),
                         color=reach_style_color_red(1))
variable_vertices<-cbind(name=unique(source),type="variable",
                         label=unique(source),
                         color=reach_style_color_darkgrey(2))
variable_vertices<-variable_vertices[!(variable_vertices[,"name"] %in% composite_vertice[,"name"]),]
vertex_list<-rbind(variable_vertices,condition_vertices,composite_vertice)


# compile graph
g<-graph_from_data_frame(el,directed = T,vertices = vertex_list)
# composite_edgelist<-composite_indicators_definitions_weighted_counts[,c("var","new.var.name")]
# condition_edgelist$type<-"condition"
# compiled_edgelist<-rbind(condition_edgelist %>% as.matrix,composite_edgelist %>% as.matrix)

# vertices_composite<-cbind(unique(as.vector(composite_edgelist[,2])),"composite")
# vertices_variable<-cbind(unique(as.vector(composite_edgelist[,1]),"composite")
# vertices_condition<-cbind(unique(condition_edgelist)[,1])

# g<-graph.edgelist(compiled_edgelist) %>% simplify

g_reversed<-graph.edgelist(el[,2:1]) 
pagerank<-page.rank(g_reversed,directed = T,damping = 1)$vector[names(V(g))]
pagerank<-(pagerank-min(pagerank))
pagerank<-pagerank/max(pagerank)

primary<-reach_style_color_red(1)

secondary<-reach_style_color_red(3)
neutral<-"black"
size<-pagerank
pdf(file = "./output/composite_indicator_visualisation/indicator_graph.pdf",18,18)

plot(g,
     # vertex.color=primary,
     vertex.frame.color=NA,
     vertex.size=(size+1)*15,
     vertex.label.color=neutral,
     edge.color="#CCCCCC",
     edge.width=2,edge.curved=F,vertex.label.dist=0)

dev.off()



}



