source(file.path("src", "graph.R"))

# draw the minimization surface graph image
df = read.table(file.path("analysis", "min-surf-img", "individual.csv"), header=TRUE, sep=",")
df_paired = read.table(file.path("analysis", "min-surf-img", "paired.csv"), header=TRUE, sep=",")
NUM_PREDICTORS = 3
graph = create_pgraph(NUM_PREDICTORS)
sizes = df$std_mse_sqrt
smallest = min(sizes)
range = max(sizes) - smallest
# scale sizes so that we get sizes from 10 to 40
sizes = sizes * 32 / range - smallest + 8
print(df$predictors)
label = unlist(df$predictors)
colors()
edge_label = round(df_paired$RESPONSE_VARIABLExstd_mse_diff, digits = 1)
plot(graph, vertex.size=sizes, vertex.color="lightgrey",
    vertex.label=df$predictors, vertex.label.family="sans", vertex.label.color="black",
    edge.label=edge_label, edge.label.family="sans", edge.label.color="black")