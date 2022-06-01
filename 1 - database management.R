rm(list = ls())

#### > 0. Packages ####
libs <- c(
  'tidyverse', 
  'DBI', 'RPostgreSQL',
  'readxl'
)
invisible(lapply(libs, library, character.only = T))

# Connect to database
db = dbConnect(drv = dbDriver("PostgreSQL"),
                     host = "000.00.00.0.",
                     port = "0000",
                     user = "username",
                     password = "pwd",
                     dbname = "database.name")

# Table to all and update in the database
df = read_xlsx('table.xlsx', sheet = 'sheet.name')

#### > 1. Management functions ####

#### >> 1.1. Add missing data from table ####
#### >> 1.1. Identify missing primary keys (PK) ####
# Extract database entrees in "table"
db.entries = 
  tbl(db, 'table') %>% 
  select(PK) %>% 
  collect() %>%
  unlist()

# Compare the the data in the df
to.add = df[!(df$PK %in% db.entries),]

# If several PK first create a combined one to identify missing entrees

# Add missing entrees
if(nrow(to.add) > 0){
  app = sqlAppendTable(con = db, 
                       table = "table",
                       value = to.add, 
                       row.names = F)
  dbExecute(con = db,
            statement = app)
}

#### >> 1.2. Compare to table and update when necessary ####
for(i in 1:nrow(df)){
  # For each PK in df
  id = df$PK[i]
  # Extract the entree from the db
  db.data = 
    tbl(db, 'table') %>%
    filter(PK == id) %>%
    collect()
  
  # Compare to df entree
  df.data = df[i,]
  
  if(sum(db.data != df.data) > 0){
    changes = which(db.data != df.data)
    # Update database
    for(j in changes){
      # Select changed column
      column.changed = colnames(df[,j])
      # Write SQL statement
      statement = 
        paste0("UPDATE table
        SET 
        ",column.changed, " = '", df[i,j],"'
        WHERE PK = '", id, "';"
        )
      # Execute change in the database
      dbExecute(conn = workshop,
                statement = statement)
    }
  }
}
#### > 2. Visualization ####

#### >> 2.1. Extract relationships #### 
dm_db = db %>% 
  dbGetQuery(., 
             dm_re_query("postgres")) %>% 
  as.data_model()

#### >> 2.2. remove duplicated artifacts ####
dm_db$columns = dm_db$columns %>% 
  dplyr::select(-ref_col) %>% 
  distinct_all()

#### >> 3.4 Plot ####
graph = dm_create_graph(dm_db,
                        rankdir = 'BT',
                        graph_attrs = "rankdir = RL, bgcolor = '#F4F0EF' ", 
                        edge_attrs = "dir = both, arrowtail = crow, arrowhead = odiamond",
                        node_attrs = "fontname = 'Arial'")


graph

#### > 3. Disconnect database ####
dbDisconnect(workshop)
#### END ####