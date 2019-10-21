################################################################################################
#### This code is for webscraping Basketball-Reference player data
## Created: October 5, 2019
## Edited:
################################################################################################

rm(list = ls())

library(tidyverse) # data wrangling
library(janitor) # data cleaning
library(rvest) # webscraping
library(corrplot) # correlation plots
library(factoextra) # pca & clustering
library(bsplus) # for reading html
library(tidyr)
library(dplyr)
library(broom)
library(purrr)
library(igraph) # network data structures and tools
library(ggraph) # network visualization

# set working directory
setwd("/Users/m/Desktop/Basketball/Projects/Webscrape/R/Players")



################################################################################################
# The Function
################################################################################################
scrape_stats <- function(season = 2019){
  # scrape
  # feed in the url
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  # grab table data
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  # clean the data in one fell swoop
  player_stats <- stats_tot %>% 
    remove_empty_cols() %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  return(player_stats)
}









################################################################################################
# Work done to get to the Function
################################################################################################
# feed in the url
url <- 'https://www.basketball-reference.com/leagues/NBA_2019_totals.html'

# grab the table data
stats <- url %>%
  read_html() %>%
  html_table() %>%
  .[[1]]

# take a look at the table data
str(stats)

# clean the data
stats <- stats %>%
  remove_empty_cols() %>%
  clean_names() %>%
  dplyr::filter(!player=='Player') %>% # delete headers
  mutate_at(vars(-c(player, tm, pos)), as.numeric) %>% # turn all stat cols to numeric
  mutate_at(vars(-c(player, tm , pos)), funs(replace(., is.na(.), 0))) %>% # converts NAs
  as_tibble()
str(stats)

# convert traded players to total statistics
stats <- stats %>%
  group_by(player) %>%
  slice(1) %>%
  ungroup()

# remove rank
stats <- stats[,-1]












################################################################################################
# The Final Function - scrape + combine 3 types of player stats
################################################################################################

scrape_stats <- function(season = 2019){
  ######################
  # SCRAPE TOTAL STATS #
  ######################
  # feed in the url
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  # grab table data
    stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
    # clean the data in one fell swoop
  player_stats_tot <- stats_tot %>% 
    remove_empty_cols() %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  #######################
  # SCRAPE PER 36 STATS #
  #######################
  # feed in the url
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  # grab table data
    stats_pm <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  # clean the data in one fell swoop
    player_stats_pm <- stats_pm %>% 
    remove_empty_cols() %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
    select(-rk)
  
  #########################
  # SCRAPE ADVANCED STATS #
  #########################
  # feed in the url  
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
    # grab table data
        stats_adv <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
    # clean the data in one fell swoop
    player_stats_adv <- stats_adv %>% 
    remove_empty_cols() %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  # join the data
  player_stats <- full_join(player_stats_tot,player_stats_pm,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
    full_join(player_stats_adv,
              by = c("player", "pos", "age", "tm", "g", "mp"))
  return(player_stats)
}











################################################################################################
# diving a little deeper - determining player positions
################################################################################################
?map_dfr()

# get the data for years between 2014-2019 the 'current NBA era'
player_stats_modern <- map_dfr(2014:2019, scrape_stats)

# reduce noise by eliminating players that played under my specified minutes threshold
player_stats_modern <- player_stats_modern %>%
  dplyr::filter(mp>=1500) # more than 250 min per season
# drops from just over 3000 players to 1139

# reduce dimensionality
pca_nba_modern <- player_stats_modern %>%
  select(fg:vorp) %>% # basically select all the columns of interest
  as.matrix() %>% # self-explanatory
  prcomp(center=T, scale=T, retx=T) # perform PCA - get covariance matrix

# scree plot
quartz()
fviz_eig(pca_nba_modern, ncp=15) # looks like between 4-6 components, but let's take a closer look


player_stats_ld <- pca_nba_modern$x[,1:10]

nba_var_modern <- get_pca_var(pca_nba_modern)
pcs_modern <- nba_var_modern$contrib[, 1:10]
colnames(pcs_modern) <- paste0('PC', 1:10)


as_tibble(pcs_modern, rownames='stat') %>%
  gather(pc, contrib, PC1:PC10) %>%
  mutate(pc=factor(pc, levels=paste0("PC", 1:10))) %>%
  group_by(pc) %>%
  top_n(5, contrib) %>%
  ggplot(aes(x=stat, y=contrib)) + geom_col() + facet_wrap(~pc, scales='free', ncol=5) + labs(x='', y='')
# returns more emphasis on defensive rebounding than I would have anticipated

# determine the optimal number of clusters based on 'average silhoutte'
fviz_nbclust(player_stats_ld, kmeans, method = "silhouette") # 3 is the optimal number of clusters

#### FROM WIKIPEDIA ####
# The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation). 
# The silhouette ranges from −1 to +1, where a high value indicates that the object is well matched to its 
  # own cluster and poorly matched to neighboring clusters. 
# If most objects have a high value, then the clustering configuration is appropriate. 
# If many points have a low or negative value, then the clustering configuration may have too many or too few clusters. 


# cluster using k-means
player_clusters_modern <- kmeans(player_stats_ld, centers = 3)

player_clusters_modern$centers
as_tibble(player_clusters_modern$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:3,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")









################################################################################################
# Uniform Manifold Approximation and Projection (UMAP)
################################################################################################
# In this post, we will construct one example for a similarity network, namely a similarity network of NBA players. 
# Similarity is based on the player stats and if two players are connected in the network, 
  # then they can be considered to be of the same player type. 
# I am not the first to do this. There has been a talk at the SLOAN Conference by Muthu Alagappan who seemed 
  # to have done exactly this.
?umap

# just like before take the data we're interested in and remove the rest, then scale what remains
umap_player <- player_stats_modern %>% 
  select(fg:vorp) %>%
  as.matrix() %>% 
  scale()

# implement the UMAP and take a look at the results  
player.umap <- umap(umap_player, n_components = 10)
head(player.umap$layout)

# convert the distances between values into a matrix
  # further two players are apart, the less similar they are
D <- dist(player.umap$layout, method='euclidean', diag = TRUE, upper = TRUE) %>% 
  as.matrix()

# determine a threshold for whether players are similar
  # turn the distance matrix into a binary matrix to be used as a graphical object
?graph_from_adjacency_matrix
bin_mtx <- (D < 0.5) + 0 # convert those less than 0.5 to 0s
g <- graph_from_adjacency_matrix(bin_mtx, 'undirected', diag=F) # max, don't care if the matrix is symmetrical
V(g)$name <- player_stats_modern$player

# use ggraph and an igraph layout to plot our network
ggraph(g, layout = "kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(size = 2)+
  theme_graph() 
# this follows the thinking that there is increasingly less difference between skillsets at different positions

# let's create a new average variable to represent a metric we'll look at throughout history: 3PAr
  # percentage of FGs attempted from 3-point range
three_par_modern <- sum(player_stats_modern$x3p_ar)/length(player_stats_modern$x3p_ar) 
three_par_modern
# [1] 0.308216 about 31% of all shots are three-pointers

# get the positions of the players so we can add some color
V(g)$Position <- player_stats_modern$pos
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = Position),size = 2)+
  theme_graph()+
  theme(legend.position = "bottom")
# lots of combinations here, adding more evidence


# we can look at the per 36 minute scoring as well
V(g)$pts_pm <- player_stats_modern$pts_pm
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = pts_pm),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's look at the percentage of FGs that were 3-pointers
V(g)$x3p_ar <- player_stats_modern$x3p_ar
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = x3p_ar),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")

# let's look at wins share percentages of player types over the years
V(g)$ws_48<- player_stats_modern$ws_48
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = ws_48),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# we can look at the team that essentially kicked this era of basketball off, the GSW:
V(g)$tm <- ifelse(player_stats_modern$tm=="GSW","GSW","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("GSW"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# now let's compare them to one of the worst teams PHO:
V(g)$tm <- ifelse(player_stats_modern$tm=="PHO","PHO","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("PHO"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")























################################################################################################
# let's compare some eras
################################################################################################
# start by defining the eras we want to look at:
  # 14-19: modern 3ball
  # 05-13: pace & space
  # 98-04: slow, defensive
  # 89-98: Jordan rules
  # 79-88: Magic v Bird and the introduction of the 3 point line

# let's avoid pre-3 era just so we can take a stab at when the analytics bomb went off
# some early thoughts: 
  # the further back we look, the more spaced out the network will be
    # my assumption is that positions had more clearly defined functions
  # ppg should stay roughly the same, but how about pace or efficiency?
  # who were the dominant teams of each era and do they have any distinct differences from
    # other good teams of the era, bad teams of the era, and those in more modern times


# gather some data
# already have 14-19 seasons stored, so let's start there and work backwards







###################################
# get the data for 'pace & space' #
###################################
player_stats_pns <- map_dfr(2004:2013, scrape_stats)

# reduce noise by eliminating players that played under my specified minutes threshold
player_stats_pns <- player_stats_pns %>%
  dplyr::filter(mp>=1500) # more than 250 min per season
# drops from just over 3000 players to 1139

# reduce dimensionality
pca_nba_pns <- player_stats_pns %>%
  select(fg:vorp) %>% # basically select all the columns of interest
  as.matrix() %>% # self-explanatory
  prcomp(center=T, scale=T, retx=T) # perform PCA - get covariance matrix

# scree plot
quartz()
fviz_eig(pca_nba_pns, ncp=15) # looks like between 4-6 components, but let's take a closer look


player_stats_ld <- pca_nba_pns$x[,1:10]

nba_var_pns <- get_pca_var(pca_nba_pns)
pcs_pns <- nba_var_pns$contrib[, 1:10]
colnames(pcs_pns) <- paste0('PC', 1:10)


as_tibble(pcs_pns, rownames='stat') %>%
  gather(pc, contrib, PC1:PC10) %>%
  mutate(pc=factor(pc, levels=paste0("PC", 1:10))) %>%
  group_by(pc) %>%
  top_n(5, contrib) %>%
  ggplot(aes(x=stat, y=contrib)) + geom_col() + facet_wrap(~pc, scales='free', ncol=5) + labs(x='', y='')
# returns more emphasis on defensive rebounding than I would have anticipated

# determine the optimal number of clusters based on 'average silhoutte'
fviz_nbclust(player_stats_ld, kmeans, method = "silhouette") # 2 is the optimal number of clusters
# this flying in the face of my assumption

# cluster using k-means
player_clusters_pns <- kmeans(player_stats_ld, centers = 3)

player_clusters_pns$centers
as_tibble(player_clusters_pns$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:3,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")






# just like before take the data we're interested in and remove the rest, then scale what remains
umap_player <- player_stats_pns %>% 
  select(fg:vorp) %>%
  as.matrix() %>% 
  scale()

# implement the UMAP and take a look at the results  
player.umap <- umap(umap_player, n_components = 10)
head(player.umap$layout)

# convert the distances between values into a matrix
# further two players are apart, the less similar they are
D <- dist(player.umap$layout, method='euclidean', diag = TRUE, upper = TRUE) %>% 
  as.matrix()

# determine a threshold for whether players are similar
# turn the distance matrix into a binary matrix to be used as a graphical object
?graph_from_adjacency_matrix
bin_mtx <- (D < 0.5) + 0 # convert those less than 0.5 to 0s
g <- graph_from_adjacency_matrix(bin_mtx, 'undirected', diag=F) # max, don't care if the matrix is symmetrical
V(g)$name <- player_stats_pns$player


# percentage of FGs attempted from 3-point range
three_par_pns <- sum(player_stats_pns$x3p_ar)/length(player_stats_pns$x3p_ar) 
three_par_pns
# [1] 0.2147328 about 21.5% of all shots are three-pointers which is ~9.5% less than 'Modern Basketball'


# get the positions of the players so we can add some color
V(g)$Position <- player_stats_pns$pos
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = Position),size = 2)+
  theme_graph()+
  theme(legend.position = "bottom")
# lots of combinations here, adding more evidence


# we can look at the per 36 minute scoring as well
V(g)$pts_pm <- player_stats_pns$pts_pm
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = pts_pm),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's look at the percentage of FGs that were 3-pointers
V(g)$x3p_ar <- player_stats_pns$x3p_ar
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = x3p_ar),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")

# let's look at wins share percentages of player types over the years
V(g)$ws_48<- player_stats_pns$ws_48
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = ws_48),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's take a look at a team that was successful for ~20 years straight SAS:
V(g)$tm <- ifelse(player_stats_pns$tm=="SAS","SAS","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("SAS"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# now let's compare them to one of the worst teams of this era CHA:
V(g)$tm <- ifelse(player_stats_pns$tm=="CHA","CHA","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("CHA"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")






######################################
# get the data for 'Slow, defensive' #
######################################
player_stats_sd <- map_dfr(1999:2003, scrape_stats)

# reduce noise by eliminating players that played under my specified minutes threshold
player_stats_sd <- player_stats_sd %>%
  dplyr::filter(mp>=1500) # more than 250 min per season
# drops from just over 3000 players to 1139

# reduce dimensionality
pca_nba_sd <- player_stats_sd %>%
  select(fg:vorp) %>% # basically select all the columns of interest
  as.matrix() %>% # self-explanatory
  prcomp(center=T, scale=T, retx=T) # perform PCA - get covariance matrix

# scree plot
quartz()
fviz_eig(pca_nba_sd, ncp=15) # looks like between 4-6 components, but let's take a closer look


player_stats_ld <- pca_nba_sd$x[,1:10]

nba_var_sd <- get_pca_var(pca_nba_sd)
pcs_sd <- nba_var_sd$contrib[, 1:10]
colnames(pcs_sd) <- paste0('PC', 1:10)


as_tibble(pcs_sd, rownames='stat') %>%
  gather(pc, contrib, PC1:PC10) %>%
  mutate(pc=factor(pc, levels=paste0("PC", 1:10))) %>%
  group_by(pc) %>%
  top_n(5, contrib) %>%
  ggplot(aes(x=stat, y=contrib)) + geom_col() + facet_wrap(~pc, scales='free', ncol=5) + labs(x='', y='')
# returns more emphasis on defensive rebounding than I would have anticipated

# determine the optimal number of clusters based on 'average silhoutte'
fviz_nbclust(player_stats_ld, kmeans, method = "silhouette") # 2 is the optimal number of clusters
# this flying in the face of my assumption

# cluster using k-means
player_clusters_sd <- kmeans(player_stats_ld, centers = 3)

player_clusters_sd$centers
as_tibble(player_clusters_sd$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:3,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")






# just like before take the data we're interested in and remove the rest, then scale what remains
umap_player <- player_stats_sd %>% 
  select(fg:vorp) %>%
  as.matrix() %>% 
  scale()

# implement the UMAP and take a look at the results  
player.umap <- umap(umap_player, n_components = 10)
head(player.umap$layout)

# convert the distances between values into a matrix
# further two players are apart, the less similar they are
D <- dist(player.umap$layout, method='euclidean', diag = TRUE, upper = TRUE) %>% 
  as.matrix()

# determine a threshold for whether players are similar
# turn the distance matrix into a binary matrix to be used as a graphical object
?graph_from_adjacency_matrix
bin_mtx <- (D < 0.5) + 0 # convert those less than 0.5 to 0s
g <- graph_from_adjacency_matrix(bin_mtx, 'undirected', diag=F) # max, don't care if the matrix is symmetrical
V(g)$name <- player_stats_sd$player


# get the positions of the players so we can add some color
V(g)$Position <- player_stats_sd$pos
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = Position),size = 2)+
  theme_graph()+
  theme(legend.position = "bottom")
# lots of combinations here, adding more evidence


# percentage of FGs attempted from 3-point range
three_par_sd <- sum(player_stats_sd$x3p_ar)/length(player_stats_modern$x3p_ar) 
three_par_sd
# [1] 0.1261273 as expected, quite a drop-off



# we can look at the per 36 minute scoring as well
V(g)$pts_pm <- player_stats_sd$pts_pm
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = pts_pm),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's look at the percentage of FGs that were 3-pointers
V(g)$x3p_ar <- player_stats_sd$x3p_ar
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = x3p_ar),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")

# let's look at wins share percentages of player types over the years
V(g)$ws_48<- player_stats_sd$ws_48
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = ws_48),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's take a look at the Pistons who dethroned the 3-time defending champ Lakers at the end of this era DET:
V(g)$tm <- ifelse(player_stats_sd$tm=="DET","DET","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("DET"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# and the Lakers since they went to 4 straight finals and won 3 LAL:
V(g)$tm <- ifelse(player_stats_sd$tm=="LAL","LAL","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("LAL"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# now let's compare them to one of the worst teams of this era CLE:
V(g)$tm <- ifelse(player_stats_sd$tm=="CLE","CLE","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("CLE"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")











######################################
# get the data for 'Jordan Rules #
######################################
player_stats_jor <- map_dfr(1989:1998, scrape_stats)

# reduce noise by eliminating players that played under my specified minutes threshold
player_stats_jor <- player_stats_jor %>%
  dplyr::filter(mp>=1500) # more than 250 min per season
# drops from just over 3000 players to 1139

# reduce dimensionality
pca_nba_jor <- player_stats_jor %>%
  select(fg:vorp) %>% # basically select all the columns of interest
  as.matrix() %>% # self-explanatory
  prcomp(center=T, scale=T, retx=T) # perform PCA - get covariance matrix

# scree plot
quartz()
fviz_eig(pca_nba_jor, ncp=15) # looks like between 4-6 components, but let's take a closer look


player_stats_ld <- pca_nba_jor$x[,1:10]

nba_var_jor <- get_pca_var(pca_nba_jor)
pcs_jor <- nba_var_jor$contrib[, 1:10]
colnames(pcs_jor) <- paste0('PC', 1:10)


as_tibble(pcs_jor, rownames='stat') %>%
  gather(pc, contrib, PC1:PC10) %>%
  mutate(pc=factor(pc, levels=paste0("PC", 1:10))) %>%
  group_by(pc) %>%
  top_n(5, contrib) %>%
  ggplot(aes(x=stat, y=contrib)) + geom_col() + facet_wrap(~pc, scales='free', ncol=5) + labs(x='', y='')
# returns more emphasis on defensive rebounding than I would have anticipated

# determine the optimal number of clusters based on 'average silhoutte'
fviz_nbclust(player_stats_ld, kmeans, method = "silhouette") # 2 is the optimal number of clusters
# this flying in the face of my assumption

# cluster using k-means
player_clusters_jor <- kmeans(player_stats_ld, centers = 3)

player_clusters_jor$centers
as_tibble(player_clusters_jor$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:3,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")






# just like before take the data we're interested in and remove the rest, then scale what remains
umap_player <- player_stats_jor %>% 
  select(fg:vorp) %>%
  as.matrix() %>% 
  scale()

# implement the UMAP and take a look at the results  
player.umap <- umap(umap_player, n_components = 10)
head(player.umap$layout)

# convert the distances between values into a matrix
# further two players are apart, the less similar they are
D <- dist(player.umap$layout, method='euclidean', diag = TRUE, upper = TRUE) %>% 
  as.matrix()

# determine a threshold for whether players are similar
# turn the distance matrix into a binary matrix to be used as a graphical object
?graph_from_adjacency_matrix
bin_mtx <- (D < 0.5) + 0 # convert those less than 0.5 to 0s
g <- graph_from_adjacency_matrix(bin_mtx, 'undirected', diag=F) # max, don't care if the matrix is symmetrical
V(g)$name <- player_stats_jor$player


# percentage of FGs attempted from 3-point range
three_par_jor <- sum(player_stats_jor$x3p_ar)/length(player_stats_modern$x3p_ar)
three_par_jor
# [1] 0.1974952 interestingly, there's been an increase


# get the positions of the players so we can add some color
V(g)$Position <- player_stats_jor$pos
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = Position),size = 2)+
  theme_graph()+
  theme(legend.position = "bottom")
# lots of combinations here, adding more evidence


# we can look at the per 36 minute scoring as well
V(g)$pts_pm <- player_stats_jor$pts_pm
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = pts_pm),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's look at the percentage of FGs that were 3-pointers
V(g)$x3p_ar <- player_stats_jor$x3p_ar
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = x3p_ar),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")

# let's look at wins share percentages of player types over the years
V(g)$ws_48<- player_stats_jor$ws_48
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = ws_48),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's take a look at MJ's team for obvious reasons CHI:
V(g)$tm <- ifelse(player_stats_jor$tm=="CHI","CHI","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("CHI"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# arguably the second most successful team of that era UTA:
V(g)$tm <- ifelse(player_stats_jor$tm=="UTA","UTA","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("UTA"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# now let's compare them to one of the worst teams of this era LAC:
V(g)$tm <- ifelse(player_stats_jor$tm=="LAC","LAC","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("LAC"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")











###################################
# get the data for 'Magic v Bird' #
###################################
player_stats_mvb <- map_dfr(1980:1988, scrape_stats)

# reduce noise by eliminating players that played under my specified minutes threshold
player_stats_mvb <- player_stats_mvb %>%
  dplyr::filter(mp>=1500) # more than 250 min per season
# drops from just over 3000 players to 1139

# reduce dimensionality
pca_nba_mvb <- player_stats_mvb %>%
  select(fg:vorp) %>% # basically select all the columns of interest
  as.matrix() %>% # self-explanatory
  prcomp(center=T, scale=T, retx=T) # perform PCA - get covariance matrix

# scree plot
quartz()
fviz_eig(pca_nba_mvb, ncp=15) # looks like between 4-6 components, but let's take a closer look


player_stats_ld <- pca_nba_mvb$x[,1:10]

nba_var_mvb <- get_pca_var(pca_nba_mvb)
pcs_jor <- nba_var_mvb$contrib[, 1:10]
colnames(pcs_mvb) <- paste0('PC', 1:10)


as_tibble(pcs_mvb, rownames='stat') %>%
  gather(pc, contrib, PC1:PC10) %>%
  mutate(pc=factor(pc, levels=paste0("PC", 1:10))) %>%
  group_by(pc) %>%
  top_n(5, contrib) %>%
  ggplot(aes(x=stat, y=contrib)) + geom_col() + facet_wrap(~pc, scales='free', ncol=5) + labs(x='', y='')
# returns more emphasis on defensive rebounding than I would have anticipated

# determine the optimal number of clusters based on 'average silhoutte'
fviz_nbclust(player_stats_ld, kmeans, method = "silhouette") # 2 is the optimal number of clusters
# this flying in the face of my assumption

# cluster using k-means
player_clusters_mvb <- kmeans(player_stats_ld, centers = 3)

player_clusters_mvb$centers
as_tibble(player_clusters_jor$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:3,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")






# just like before take the data we're interested in and remove the rest, then scale what remains
umap_player <- player_stats_mvb %>% 
  select(fg:vorp) %>%
  as.matrix() %>% 
  scale()

# implement the UMAP and take a look at the results  
player.umap <- umap(umap_player, n_components = 10)
head(player.umap$layout)

# convert the distances between values into a matrix
# further two players are apart, the less similar they are
D <- dist(player.umap$layout, method='euclidean', diag = TRUE, upper = TRUE) %>% 
  as.matrix()

# determine a threshold for whether players are similar
# turn the distance matrix into a binary matrix to be used as a graphical object
?graph_from_adjacency_matrix
bin_mtx <- (D < 0.5) + 0 # convert those less than 0.5 to 0s
g <- graph_from_adjacency_matrix(bin_mtx, 'undirected', diag=F) # max, don't care if the matrix is symmetrical
V(g)$name <- player_stats_mvb$player


# get the positions of the players so we can add some color
V(g)$Position <- player_stats_mvb$pos
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = Position),size = 2)+
  theme_graph()+
  theme(legend.position = "bottom")
# lots of combinations here, adding more evidence


# percentage of FGs attempted from 3-point range
three_par_mvb <- sum(player_stats_mvb$x3p_ar)/length(player_stats_modern$x3p_ar) 
three_par_mvb
# [1] 0.03776822 not surprising given the 3-pointer was introduced in the first season we're looking at here


# we can look at the per 36 minute scoring as well
V(g)$pts_pm <- player_stats_mvb$pts_pm
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = pts_pm),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's look at the percentage of FGs that were 3-pointers
V(g)$x3p_ar <- player_stats_mvb$x3p_ar
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = x3p_ar),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")

# let's look at wins share percentages of player types over the years
V(g)$ws_48<- player_stats_mvb$ws_48
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = ws_48),size = 2)+
  scale_color_gradient(low="#104E8B", high="#CD2626")+
  theme_graph()+
  theme(legend.position="bottom")


# let's take a look at Magic's team for obvious reasons LAL:
V(g)$tm <- ifelse(player_stats_mvb$tm=="LAL","LAL","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("LAL"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# and obviously let's look at Bird's team BOS:
V(g)$tm <- ifelse(player_stats_mvb$tm=="BOS","BOS","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("BOS"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")


# now let's compare them to one of the worst teams of this era NYK:
V(g)$tm <- ifelse(player_stats_mvb$tm=="NYK","NYK","other")
ggraph(g, layout="kk")+
  geom_edge_link(colour = "grey")+
  geom_node_point(aes(color = tm),size = 2)+
  scale_color_manual(values=c("NYK"="#CD2626","other"="gray27"))+
  theme_graph()+
  theme(legend.position="bottom")






threes <- rbind(three_par_modern, three_par_pns, three_par_sd, three_par_jor, three_par_mvb)
threes
years <- 1980:2019


################################################################################################
# Team Odds
################################################################################################
# feed in the url
url <- 'https://mybookie.ag/sportsbook/nba/regular-season-wins/'


bs_accordion(url)



# grab the table data
stats <- url %>%
  read_html() %>%
  html_table() %>%
  .[[1]]

# take a look at the table data
str(stats)

# clean the data
stats <- stats %>%
  remove_empty_cols() %>%
  clean_names() %>%
  dplyr::filter(!player=='Player') %>% # delete headers
  mutate_at(vars(-c(player, tm, pos)), as.numeric) %>% # turn all stat cols to numeric
  mutate_at(vars(-c(player, tm , pos)), funs(replace(., is.na(.), 0))) %>% # converts NAs
  as_tibble()
str(stats)
}




























































