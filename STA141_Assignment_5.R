library(RSQLite)
setwd("/Users/macbook/Desktop/graduate study/2015 fall/STA 141")
#Get the tables
de = dbConnect(SQLite(), "lean_imdbpy.db")
dbListTables(de)
db = dbConnect(SQLite(), "lean_imdbpy_2010_idx.db")




####1
#Check if there is NA values in person_id
dbGetQuery(de, 'SELECT Distinct person_id FROM cast_info where person_id is NULL;') #No NAs. 
#Then get the total number of distinct person_id which equals to total number of actors
dbGetQuery(de, 'SELECT count(DISTINCT person_id) FROM cast_info WHERE role_id = 1 or role_id = 2;')  
#Check if there is NA values in movie_id
dbGetQuery(de, 'SELECT Distinct id FROM title where id is NULL;') #No NAs.
#Then get the total number of distinct movie_id which equals total number of movues
dbGetQuery(de, 'SELECT count(Distinct id) from title;') 



####2
#table Title production_year range
dbGetQuery(de, 'SELECT MAX(production_year), MIN(production_year) FROM title;') 
#table aka_title production_year range
dbGetQuery(de, 'SELECT MAX(production_year),MIN(production_year) FROM aka_title;') 



####3
dbGetQuery(de, 'SELECT b.gender, COUNT(DISTINCT a.person_id)*1.0/(SELECT COUNT(DISTINCT a.person_id) FROM cast_info AS a, name AS b 
              WHERE a.person_id = b.id AND (a.role_id = 1 OR a.role_id = 2)) AS PROPORTION
           FROM cast_info AS a, name AS b WHERE a.person_id = b.id AND (a.role_id = 1 OR a.role_id = 2)
           GROUP BY b.gender;') 


####4
#get the total number 
dbGetQuery(de, 'SELECT  kind, COUNT(DISTINCT title.id) FROM title, kind_type 
           WHERE title.kind_id = kind_type.id GROUP BY kind_id;') 
#get the proportion
dbGetQuery(de, 'SELECT  kind, COUNT(DISTINCT title.id)*1.0/(SELECT COUNT(DISTINCT title.id) FROM title, kind_type 
           WHERE title.kind_id = kind_type.id) AS PROPORTION FROM title, kind_type WHERE title.kind_id = kind_type.id GROUP BY kind_id;') 

####5
#get how many genres
dbGetQuery(de, 'SELECT COUNT(DISTINCT a.info) FROM movie_info AS a, info_type AS b WHERE b.info = \'genres\' AND (b.id = a.info_type_id);') 
genresName = dbGetQuery(de, 'SELECT DISTINCT a.info FROM movie_info AS a, info_type AS b WHERE b.info = \'genres\' AND (b.id = a.info_type_id);') 
#Get names of these genres
genresName$info 





####6
dbGetQuery(de, 'SELECT COUNT(DISTINCT a.movie_id), a.info
           FROM movie_info AS a, info_type AS b WHERE b.info = \'genres\' AND (b.id = a.info_type_id) 
           GROUP BY a.info ORDER BY COUNT(DISTINCT a.movie_id) DESC LIMIT 10;') 


####7
#get the count of movies with keyword 'space'.
dbGetQuery(de, "SELECT COUNT(DISTINCT c.id) FROM keyword AS a, movie_keyword AS b, title AS c, kind_type as d
                  WHERE a.keyword LIKE 'space' AND b.keyword_id = a.id AND b.movie_id = c.id AND c.kind_id = d.id AND d.kind = 'movie'")  

releaseyear = dbGetQuery(de, "SELECT DISTINCT c.production_year FROM keyword AS a, movie_keyword AS b, title AS c, kind_type as d
                  WHERE a.keyword = 'space' AND b.keyword_id = a.id AND b.movie_id = c.id AND c.kind_id = d.id 
                  AND d.kind = 'movie' AND c.production_year IS NOT NULL ORDER BY c.production_year; ")  
releaseyear$production_year 
#get top 5 actors 
top5 = dbGetQuery(de, "SELECT DISTINCT f.name, e.nr_order,c.title, e.person_id FROM keyword AS a, movie_keyword AS b, title AS c, cast_info as e, name AS f
                  WHERE a.keyword LIKE 'space' 
                    AND b.keyword_id = a.id 
                    AND b.movie_id = c.id 
                    AND c.kind_id = '1' 
                  AND e.movie_id = c.id 
                  AND e.person_id = f.id 
                    AND e.role_id IN (1,2)
                  AND e.nr_order BETWEEN 1 AND 5 ;") 
head(top5)

####8
movieAndTime = dbGetQuery(de, 'SELECT COUNT(DISTINCT a.movie_id) AS COUNT, a.info AS GENRES, c.production_year AS year
           FROM movie_info AS a, info_type AS b, title AS c 
           WHERE b.info = \'genres\' AND b.id = a.info_type_id AND a.movie_id = c.id AND c.kind_id = 1 AND year IS NOT NULL
           GROUP BY c.production_year, a.info 
           ORDER BY COUNT(DISTINCT a.movie_id) DESC;') 

ggplot(movieAndTime, aes(x = year, y = COUNT)) +
  geom_line() +
  facet_wrap(~GENRES) + 
  ggtitle('Number of movies in each year over time for each genre')
  

#Get the tables
db = dbConnect(SQLite(), "lean_imdbpy_2010_idx.db")
dbListTables(db)

####9
dbGetQuery(db, 'SELECT COUNT(DISTINCT a.movie_id) AS COUNT, b.name AS Actors, a.person_id AS ID
FROM cast_info2 AS a, name2 AS b, title2 AS c 
                          WHERE a.person_id = b.id AND c.kind_id = 1 AND a.movie_id = c.id AND a.role_id IN (1,2)
GROUP BY a.person_id
                          ORDER BY COUNT DESC LIMIT 20;') 
#9.	Who are the actors that have been in the most movies? List the top 20.

#Use dplyr package to do the R command part
library(dplyr)
cast = dbReadTable(db,"cast_info2")
title = dbReadTable(db,"title2")
name = dbReadTable(db,"name2")

castitle = merge(cast, title, by.x = 'movie_id', by.y = 'id')
castitle = castitle[(castitle$role_id == 1 | castitle$role_id == 2) & (castitle$kind_id ==1),]
q9 =castitle %>%
  group_by(person_id) %>%
  summarise(movie = length(unique(movie_id)))%>%
  arrange(desc(movie))%>%
  head(20)

rownames(name) = name$id
name[as.character(q9$person_id),2]


####10
dbGetQuery(db, 'SELECT a.person_id AS ID, b.name, a.nr_order,MAX(c.production_year) AS MAX_YEAR, MIN(c.production_year) AS MIN_YEAR, COUNT(DISTINCT a.movie_id) AS COUNT 
FROM cast_info2 AS a, name2 AS b, title2 AS c 
WHERE a.person_id = b.id AND c.kind_id = 1 AND a.movie_id = c.id AND a.role_id IN (1,2)
AND a.nr_order BETWEEN 1 AND 3
GROUP BY a.person_id
ORDER BY COUNT DESC LIMIT 10;')

castitle10 = castitle[castitle$nr_order == 1 | castitle$nr_order == 2 | castitle$nr_order == 3,]
q10 = castitle10  %>%
  group_by(person_id) %>%
  summarise(movie = length(unique(movie_id)), from = min(production_year), to = max(production_year))%>%
  arrange(desc(movie))%>%
  head(10)

name[as.character(q10$person_id),2]


####11
dbSendQuery(db, 'CREATE TEMPORARY TABLE top10 AS SELECT a.person_id, b.name, c.production_year,  COUNT(DISTINCT a.movie_id) AS COUNT
FROM cast_info2 AS a, name2 AS b, title2 AS c 
WHERE a.person_id = b.id AND c.kind_id = 1 AND a.movie_id = c.id AND a.role_id IN (1,2)
GROUP BY a.person_id, c.production_year
ORDER BY COUNT(DISTINCT a.movie_id) DESC LIMIT 10;')


dbGetQuery(db, 'SELECT DISTINCT a.person_id, movie_id, c.title, b.name, c.production_year
FROM cast_info2 AS a, name2 AS b, title2 AS c , top10
           WHERE c.kind_id = 1 AND a.role_id IN (1,2) AND a.movie_id = c.id AND
           a.person_id = top10.[a.person_id] AND b.name = top10.[b.name] 
           AND c.production_year = top10.[c.production_year] 
           ORDER BY a.movie_id LIMIT 10;')


q11 = castitle %>%
  group_by(person_id, production_year) %>%
  summarise(movie = length(unique(movie_id)))%>%
  ungroup()%>%
  arrange(desc(movie))%>%
  head(10)

name[as.character(q11$person_id),2]

castitle11 = castitle
castitle11$str = paste(castitle11$person_id,castitle11$production_year)
q11$str = paste(q11$person_id, q11$production_year)
table11 = do.call(rbind, lapply(q11$str, function(i) castitle11[castitle11$str == i,]))
head = head(table11[order(table11$movie_id),c("person_id","production_year", "title")],10)
cbind(head, name[as.character(head$person_id),2])





###12
dbGetQuery(db, 'SELECT COUNT(a.name) AS COUNT, b.name, a.person_id 
FROM aka_name2 AS a, name2 AS b
           WHERE a.person_id = b.id 
GROUP BY a.person_id
           ORDER BY COUNT DESC LIMIT 10;') 

aka = dbReadTable(db,"aka_name2")
akaname = merge(aka, name, by.x = 'person_id', by.y = 'id')
head(aka)

q12 = akaname %>% 
  group_by(person_id)%>%
  summarise(count = length(name.x))%>%
  arrange(desc(count))%>%
  head(10)
q12
name[as.character(q12$person_id),2]


##13

dbGetQuery(db, 'SELECT a.person_id, b.name, c.production_year,  COUNT(DISTINCT a.movie_id) AS COUNT, MIN(a.nr_order)
           FROM cast_info2 AS a, name2 AS b, title2 AS c 
           WHERE a.person_id = b.id AND c.kind_id = 1 AND a.movie_id = c.id 
           AND a.role_id IN (1,2)
           GROUP BY a.person_id
           HAVING COUNT = 20 LIMIT 5;')

saberMovie = dbGetQuery(db, 
           'SELECT DISTINCT b.title, b.production_year, a.movie_id
           FROM cast_info2 AS a, title2 AS b
           WHERE b.kind_id = 1 AND a.person_id = 1942 AND a.role_id IN (1,2) AND a.movie_id = b.id;')

dbSendQuery(db, 'CREATE TEMPORARY TABLE saberMovie AS SELECT DISTINCT b.title, b.production_year, a.movie_id
           FROM cast_info2 AS a, title2 AS b
           WHERE b.kind_id = 1 AND a.person_id = 1942 AND a.role_id IN (1,2) AND a.movie_id = b.id;')


otherActors = 
dbGetQuery(db,  'SELECT DISTINCT a.person_id, b.name
           FROM cast_info2 AS a, name2 AS b , saberMovie AS d
           WHERE a.role_id IN (1,2) AND a.movie_id = d.movie_id AND a.person_id = b.id;')


dbSendQuery(db, 'CREATE TEMPORARY TABLE otherActors AS SELECT DISTINCT a.person_id, b.name
           FROM cast_info2 AS a, name2 AS b , saberMovie AS d
           WHERE a.role_id IN (1,2) AND a.movie_id = d.movie_id AND a.person_id = b.id;')

otherMovies = dbGetQuery(db,  'SELECT DISTINCT a.movie_id, c.title   
                         FROM cast_info2 AS a, title2 AS c , otherActors AS d
                         WHERE d.person_id = a.person_id AND a.movie_id = c.id AND c.kind_id = 1 AND a.role_id IN (1,2);')  
                         
dbSendQuery(db, 'CREATE TEMPORARY TABLE otherMovies AS SELECT DISTINCT a.movie_id, c.title   
                         FROM cast_info2 AS a, title2 AS c , otherActors AS d
                         WHERE d.person_id = a.person_id AND a.movie_id = c.id AND c.kind_id = 1 AND a.role_id IN (1,2);')

finalActors = 
  dbGetQuery(db,  'SELECT a.person_id, b.name, MIN(nr_order) AS billing 
             FROM cast_info2 AS a, name2 AS b , otherMovies AS d
             WHERE a.role_id IN (1,2) AND a.movie_id = d.movie_id AND a.person_id = b.id 
             GROUP BY a.person_id;')

final = 
finalActors[finalActors$billing <=3 & finalActors$billing >=1 & !is.na(finalActors$billing),]

finalMovie =  dbGetQuery (db,  paste0('SELECT movie_id, person_id
             FROM cast_info2 
                         WHERE role_id IN (1,2) AND person_id IN (' ,paste(final$person_id, collapse = "," ), ') 
                      ;'))


n = dim(final)[1]
network = matrix(0,n,n)
colnames(network) = final$person_id
rownames(network) = final$person_id
for(i in 1:n){
  for(j in 1:n){
    movie1 = finalMovie[finalMovie$person_id == colnames(network)[i], 1] 
    movie2 = finalMovie[finalMovie$person_id == rownames(network)[j], 1] 
    if (length(intersect(movie1, movie2) != 0))
       {network[i,j] =1 } 
  }
}
    
network = network - diag(n)




library(igraph)
 # build a graph from the above matrix
graph_work = graph.adjacency(network, weighted=T, mode = "undirected")
# remove loops
graph_work = simplify(graph_work)
V(graph_work)$label = NA
V(graph_work)$degree = degree(graph_work)
vertexSize = 5 * V(graph_work)$degree / max(V(graph_work)$degree)+ .2
V(graph_work)$color = 'grey60'
# plot the graph in layout1
E(graph_work)$width=.2
plot.igraph(graph_work, layout=layout.random,edge.color="grey10", vertex.size = vertexSize, vertex.shape = 'circle', main = 'networking plot')
  





#14
dbGetQuery(db, 'SELECT COUNT(DISTINCT a.person_id) AS COUNT, c.title AS movie 
           FROM cast_info2 AS a, title2 AS c 
           WHERE  c.kind_id = 2 AND a.movie_id = c.id 
           AND a.role_id IN (1,2) AND a.person_id IN (SELECT DISTINCT cast_info2.person_id FROM title2, cast_info2
WHERE title2.kind_id = 1 AND cast_info2.movie_id = title2.id AND (nr_order IN (1,2)) AND cast_info2.role_id IN (1,2))
           GROUP BY c.id
           ORDER BY COUNT DESC
           LIMIT 10;')



