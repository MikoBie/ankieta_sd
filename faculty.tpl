```{r topic-<% tidx %>, results='asis', echo=FALSE}
topic <- set_topic(<% tidx %>, "<% tname %>")$idx
df <- get_topic_df(ddf_reduced, topic)
tn <- nrow(df)
cat(sprintf("## Topic %d: %s", topic + 1, "<% tname %>"))
```
These articles generated most reactions on Facebook in how many likes they generated (Likes), how many times they were shared (Shares) and how many times they were commented (Comments). These three measures were combined to produce Interactivity on social media score (INI, see \@ref(eq:ini)). The number of shares, Likes, and Comments is displayed next to each article. The list shows the articles that generated most interactivity.

**NOTE: Pressing the “Click Here” next to an article in the table below, displays the original article.**

```{r topic-<% tidx %>-articles, fig.width=10, fig.height=8, layout="l-body-outset"}
number <- <% tidx %>
make_topic_table(df, column = ini, interactivity = TRUE)
make_topic_table(df, n = 10, column = ini, interactivity = TRUE, descending = FALSE)
```
### WordCloud
```{r topic-<% tidx %>-wordcloud, fig.width=10, fig.height=8, layout="l-body-outset", fig.cap=cap}
make_topic_wordcloud(tdf_reduced, topic, scale = c(7, .4))
cap <- "Wordcloud of the most frequent words in the topic. This graphics shows only the frequency of words, the bigger the more frequently it was used. The position of words is random."
```
### Entity network

Nodes depict the most frequent entities in the whole corpus. Edges point from active agents (subjects) to passive agents (objects) connected by an action (verb). The size of the node is a function of the number of its connections. The color refers to the community detection method based on the random walks algorithm. It means that nodes in the same color are densely connected within color than with nodes with a different color. 75% of the weakest connections were filtered out for the sake of the visualization.

```{r topic-<% tidx %>-network, fig.width=10, fig.height=8, layout="l-body-outset", fig.cap="Entity network."}
sdf <- svo %>%
    filter(artid %in% df$id)
if (nrow(sdf) > 0) {
    sdf %>%
        make_entity_graph %>%
        viz_entity_graph(qthreshold = .75) %>%
        print
}
```
### Change over time

Interactivity Index was log-transformed for the sake of the visualization and to normalize its distribution. The plot below shows how \@ref(eq:sent) and Interactivity Index \@ref(eq:ini) changed over time in the topic.

```{r topic-<% tidx %>-si-1, fig.width=8, fig.height=8, layout="l-body-outset", fig.cap="Sentiment and Interactivity Index change over time."}
number <- <% tidx %>
make_ts_plot(df, colors = COLORS)
```

### Sentiment and Interactivity Index

Interactivity Index was log-transformed for the sake of the visualization and to normalize its distribution. Each point depicts an article. The coordinates of the labels depict the average Sentiment \@ref(eq:sent) of a given source and the average Interactivity Index \@ref(eq:ini) of a given source.

```{r topic-<% tidx %>-si-2, fig.width=8, fig.height=4, layout="l-body-outset",fig.cap=cap}
number <- <% tidx %>
make_ds_plot(df, colors = COLORS)
cap <- sprintf("Correlogram of Sentiment and Interactivity Index. Spearman correlation: %.4f. This correlogram displays the relationship between the sentiment and Interactivity Index, answering the question if topics with the average positive or negative sentiment attract more attention. Spearman correlation ranging from -1 to +1  provides a measure of the relationship. Positive values indicate positive relationships – topics with positive sentiment attract more attention while negative values indicate that topics with negative sentiment attract more attention.", cor(df$sentiment, df$ini, method = "spearman"))
```

Relative Variance is a dispersion index of the distribution.

```{r topic-<% tidx %>-si-3, fig.width=8, fig.height=4, layout="l-body-outset",fig.cap=cap}
make_sent_hist(df)
rv <- relvar(df$sentiment)
cap <- sprintf("Histogram of the articles' sentiment in the topic. Relative variance: %.4f. The relative variance of sentiment shows the distribution of sentiment of all the articles. The left peak shows the frequency of articles with the specific intensity of negative sentiment, the right peak shows the relative frequency of the articles with a positive sentiment. The high variance may indicate that the topic is controversial (some love it, some hate it) or that there are both very positive aspects and very negative aspects e.g. some articles describe how bad is the epidemics, some describe how heroic are the individuals fighting the epidemics.", rv)
```

### Semantic space of dyad

The plot portrays a semantic space of relations between directed dyads of actors  - subject and objects of sentences. For each pair of actors that appear in the same sentence, we analyze the verb connecting the actors  (e.g., A accuses B, or A helps B). For each dyad we compute an average set of verbs connecting them and based on this each dyad can be positioned in high dimensional (300 D) semantic vector space. Then, for the sake of visualization, we reduce the space to a 2-dimensional plane based using Principal Component Analysis.  This allows us to derive a crude spatial representation of the types of actions connecting the actors.  The semantic space of relationships is then divided into three general types of od relationships (clusters), where each type is depicted by a specific color.

The nature of the relationship between actors in each cluster can be interpreted by the following analysis of the most frequent verbs in each cluster.

This allows us to compare relations between actors, for example, the relation between the US and UK is described with similar verbs as the relationship between Spain and Italy but differently than the relationship between Bulgaria and EU. Colors encode cluster assignments based on the K-means method.

```{r topic-<% tidx %>-derive-semantic-space}
take_average <- function(x) {
    n <- length(x)
    reduce(x, ~.x + .y) / n
}

semdf <- svo_reduced %>%
    filter(tm_topic == topic) %>%
    filter(!is.na(subj_entity), !is.na(obj_entity)) %>%
    group_by(subj_entity, obj_entity) %>%
    summarize(
        verb_vector = list(take_average(verb_vector)),
        weight = n()
    ) %>%
    ungroup %>%
    arrange(desc(weight)) %>%
    unnest %>%
    group_by(subj_entity, obj_entity) %>%
    mutate(col = str_c("d", seq_along(subj_entity))) %>%
    spread(key = col, value = verb_vector) %>%
    ungroup

pca <- PCA(select(semdf, d1:d300), ncp = 2, graph = FALSE)

set.seed(303)
ssdf <- select(semdf, subj_entity:obj_entity) %>%
    mutate(
        diad = str_c(subj_entity, "-", obj_entity),
        D1 = pca$ind$coord[, 1],
        D2 = pca$ind$coord[, 2]
    ) %>%
    mutate(
        cluster = as.factor(kmeans(select(., D1, D2), centers = 3L, nstart = 5)$cluster)
    )

vdf <- svo_reduced %>%
    filter(tm_topic == topic) %>%
    filter(!is.na(subj_entity), !is.na(obj_entity)) %>%
    group_by(subj_entity, obj_entity) %>%
    summarize(verb = list(verb_drive)) %>%
    ungroup %>%
    mutate(diad = str_c(subj_entity, "-", obj_entity)) %>%
    left_join(., select(ssdf, diad, cluster), by = "diad") %>%
    unnest %>%
    group_by(cluster) %>%
    summarize(verb = list(verb)) %>%
    ungroup %>%
    unnest %>%
    group_by(cluster, verb) %>%
    summarize(n = n()) %>%
    ungroup %>%
    arrange(desc(n)) %>%
    split(.$cluster) %>%
    map(head, n = 15) %>%
    bind_rows
```

```{r topic-<% tidx %>-semantic-space, fig.width=10, fig.height=8, fig.cap="Semantic Space of object-subject dyads."}
ssdf %>%
    filter(semdf$weight >= quantile(semdf$weight, probs = .5)) %>%
    filter(subj_entity != obj_entity) %>%
    ggplot(aes(x = D1, y = D2, color = cluster)) +
    geom_label_repel(aes(label = diad)) +
    scale_x_continuous(limits = range(ssdf$D1)*1.1) +
    scale_y_continuous(limits = range(ssdf$D2)*1.1) +
    guides(color = "none") +
    xlab(sprintf("Dim. 1 (%.2f%%)", pca$eig[1, 2])) +
    ylab(sprintf("Dim. 2 (%.2f%%)", pca$eig[2, 2]))
```

### Most typical verbs for each cluster

To facilitate interpretation below we show the most frequent verbs for each cluster (the color coding is analogous to the one on the semantic space plot). The plot shows the frequency of verbs used to describe relationships between entities in the Semantic Space of subject-objects dyads. The colors correspond to the colors in \@ref(fig:semantic_space).

The nature of the relationship between actors in each cluster can be interpreted by the following analysis of the most frequent verbs connecting actors in each cluster.

```{r topic-<% tidx %>-semantic-clusters-words, fig.width=8, fig.height=10, fig.cap="Most typical vers in semantic clusters."}
vdf %>%
    mutate(verb = fct_reorder(verb, n, .desc = FALSE)) %>%
    ggplot(aes(x = verb, y = n, fill = cluster)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~cluster, nrow = 3, scales = "free") +
    coord_flip() +
    xlab("") +
    ylab("Verb frequency")
```


```{r topic-<% tidx %>-break, results='asis'}
cat("\n")
```
