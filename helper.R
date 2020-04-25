yellow = "#f3ce13" # the theme color?

# initialize upper and lower limit variables
min_year_all = 0
max_year_all = 0
min_decade_all = 0
max_decade_all = 0
min_runtime_all = 0
max_runtime_all = 0

# read in the upper and lower limit variables from app.R
load_data = function(min_ya, max_ya, min_da, max_da, min_ra, max_ra) {
    min_year_all = min_ya
    max_year_all = max_ya
    min_decade_all = min_da
    max_decade_all = max_da
    min_runtime_all = min_ra
    max_runtime_all = max_ra
}

########################## FUNCTIONS FOR COUNT/DISTRIBUTION TABLES BELOW #####################################
########################## FUNCTIONS FOR COUNT/DISTRIBUTION TABLES BELOW #####################################

# function to get a count of movies from each year as a table (same as above)
number_films_per_year = function(table) {

    # create empty dataframe of all years in range from calculated min to max
    all_years <- data.frame(formatC(min_year_all:max_year_all, width = 2), 0)
    names(all_years)[1] <- "year"
    names(all_years)[2] <- "count"
    all_years$year <- c(min_year_all:max_year_all)

    # make dataframe of available years and their frequency
    by_year <- table %>% group_by(year) %>% summarize(count = n())

    # join the counts into the full range of years dataframe
    by_year <- full_join(all_years, by_year, by = "year")
    by_year[is.na(by_year)] <- 0
    by_year$count.x <- NULL
    names(by_year)[2] <- "count"

    # output info
    total_films = as.numeric(sum(by_year$count))
    print(paste0("---- total counted= ", total_films))
    unique_years = as.numeric(count(by_year))
    print(paste0("---- unique years= ", unique_years))

    # return the table of counts per year
    by_year
}

get_decade = function(year){  # given a year, return its decade
    as.integer(year/10) * 10
}

# function to get a count of movies from each decade as a table
number_films_per_decade = function(table) {
    
    table_with_decade = table %>% 
        select(year) %>% 
        mutate(decade=get_decade(year)) %>%
        group_by(decade) %>%
        summarise(count=n()) %>%
        arrange(decade)
    
    
    # return the table of counts per decade
    table_with_decade
}

# function to get a count of movies from each month as a table
number_films_per_month = function(table) {

    # get a count of movies from each month
    by_month <- table %>% group_by(month) %>% summarize(count = n())

    # reorder months to be in order
    by_month$month <- factor(by_month$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
    by_month <- by_month[order(by_month$month), ]

    # output info
    print(paste0("---- total counted= ", sum(by_month$count)))
    unique_months = as.numeric(count(by_month))
    print(paste0("---- unique months= ", unique_months))

    # return the table of counts per month
    by_month
}

# function to get a distribution of runtimes as a table
distribution_of_runtimes = function(table) {

    # create empty dataframe of all runtimes in range from min to max
    all_runtimes <- data.frame(formatC(min_runtime_all:max_runtime_all, width = 2), 0)
    names(all_runtimes)[1] <- "runtime"
    names(all_runtimes)[2] <- "count"
    all_runtimes$runtime <- c(min_runtime_all:max_runtime_all)

    # make dataframe of available runtimes and their frequency
    by_runtime <- table %>% group_by(runtime) %>% summarize(count = n())

    # join the counts into the full range of years dataframe
    by_runtime <- full_join(all_runtimes, by_runtime, by = "runtime")
    by_runtime[is.na(by_runtime)] <- 0
    by_runtime$count.x <- NULL
    names(by_runtime)[2] <- "count"

    # output info
    print(paste0("---- total counted= ", sum(by_runtime$count)))
    unique_runtimes = as.numeric(count(by_runtime))
    print(paste0("---- unique runtimes= ", unique_runtimes))

    # return the distribution of runtimes
    by_runtime
}

# function to get a distribution of certificates as a table
distribution_of_certificates = function(table) {

    # get a distribution of certificate
    by_certificates <- table %>% group_by(rating) %>% summarize(count = n())

    # output info
    print(paste0("---- total counted= ", sum(by_certificates$count)))
    unique_certificates = as.numeric(count(by_certificates))
    print(paste0("---- unique certificates= ", unique_certificates))

    # return the distribution of certificates
    by_certificates
}

# function to get a distribution of genres as a table
distribution_of_genres = function(table) {

    # get a distribution of genres
    by_genre <- table %>% group_by(genre) %>% summarize(count = n())

    # output info
    print(paste0("---- total counted= ", sum(by_genre$count)))
    unique_genres = as.numeric(count(by_genre))
    print(paste0("---- unique genres= ", unique_genres))

    # return the distribution of genres
    by_genre
}

# function to get a distribution of all keywords
distribution_of_keywords = function(table) {
    
    # get a distribution of top n keywords
    by_keywords <- table %>% 
        group_by(keyword) %>% 
        summarize(count = n()) %>%
        arrange(desc(count))
    
    print(paste0("---- total counted= ", sum(by_keywords$count)))
    unique_keywords = as.numeric(count(by_keywords))
    print(paste0("---- unique keywords= ", unique_keywords))
    
    # return the distribution of top n keywords
    by_keywords
}


########################## FUNCTIONS FOR OVERVIEW YEAR PLOTS #####################################
########################## FUNCTIONS FOR OVERVIEW YEAR PLOTS #####################################

plotYearlyFilms = function(data){
    films_per_year= number_films_per_year(data) %>% filter(count > 0) 
    
    use_factor = if (nrow(films_per_year) > 1) FALSE else TRUE # to check if only single year
    
    ggplot(films_per_year, aes(x=if (use_factor) as.factor(year) else year, y=count)) +
        geom_bar(stat = "identity", fill = "gray36") + 
        labs(title = "Number of Films Per Year", x = "Year", y = "Number of Films") +
        theme(text = element_text(size=20))
}




plotMonthPerGivenYear <- function(data, year)
{
    monthly_films_all = number_films_per_month(data)
    
    if (is.null(year)) # no year given, so all the data
    { 
        ggplot(monthly_films_all, aes(x=month, y=count)) +
            geom_bar(stat = "identity", fill = "#f3ce13") + 
            labs(title = "Monthly Distribution", x = "Month", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            )
    } 
    else # year given, need to compare year with overall
    { 
        year_films = data[which(data$year == year),]
        
        monthly_films_year = number_films_per_month(year_films) %>% mutate(compare=year)
        
        monthly_films_all = monthly_films_all %>% mutate(compare='All')
        
        combined_data = rbind(monthly_films_year, monthly_films_all)
        
        ggplot(combined_data, aes(x=month, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Monthly Distribution", x = "Month", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            )
        
    }
}  



plotRuntimePerGivenYear <- function(data, year)
{
    runtime_distribution_all = distribution_of_runtimes(data)
    
    if (is.null(year)) # no year given, so all the data
    { 
        ggplot(runtime_distribution_all, aes(x=runtime, y=count)) +
            geom_point() + labs(title = "Runtime Distribution", x = "Runtime", y = "Number of Films") +
            theme(text = element_text(size=20))
    } 
    else # year given, need to compare year with overall
    { 
        year_films = data[which(data$year == year),]
        
        runtime_distribution_year =  distribution_of_runtimes(year_films) %>% mutate(compare=year)
        
        runtime_distribution_all = runtime_distribution_all %>% mutate(compare='All')
        
        combined_data = rbind(runtime_distribution_year, runtime_distribution_all)
        
        ggplot(combined_data, aes(x=runtime, y=count, color=compare)) +
            geom_point() + labs(title = "Runtime Distribution", x = "Runtime", y = "Number of Films") +
            theme(text = element_text(size=20))
        
    }
    
}


plotGenrePerGivenYear <- function(data, year)
{
    genre_distribution_all = distribution_of_genres(data)
    
    if (is.null(year)) # no year given, so all the data
    { 
        ggplot(genre_distribution_all, aes(x=genre, y=count)) +
            geom_bar(stat='identity') + 
            labs(title = "Genre Distribution", x = "Genre", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
    } 
    else # year given, need to compare year with overall
    { 
        year_films = data[which(data$year == year),]
        
        genre_distribution_year =  distribution_of_genres(year_films) %>% mutate(compare=year)
        
        genre_distribution_all = genre_distribution_all %>% 
            filter(genre %in% genre_distribution_year$genre) %>% 
            mutate(compare='All')
        
        combined_data = rbind(genre_distribution_year, genre_distribution_all)
        
        ggplot(combined_data, aes(x=genre, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Genre Distribution", x = "Genre", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
        
    }
}

plotCertificatesPerGivenYear <- function(data, year)
{
    certificate_distribution_all = distribution_of_certificates(data)
    
    if (is.null(year)) # no year given, so all the data
    { 
        
        ggplot(certificate_distribution_all, aes(x=rating, y=count)) +
            geom_bar(stat='identity', fill = "#f3ce13") + 
            labs(title = "Certificate Distribution", x = "Certificate", y = "Number of Films") + coord_flip() + 
            theme(text = element_text(size=20))
    } 
    else # year given, need to compare year with overall
    { 
        year_films = data[which(data$year == year),]
        
        certificate_distribution_year =  distribution_of_certificates(year_films) %>% mutate(compare=year)
        
        certificate_distribution_all = certificate_distribution_all %>% 
            filter(rating %in% certificate_distribution_year$rating) %>% 
            mutate(compare='All')
        
        combined_data = rbind(certificate_distribution_year, certificate_distribution_all)
        
        ggplot(combined_data, aes(x=rating, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Certificate Distribution", x = "Certificate", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
        
    }
}

plotTopKeywordsPerGivenYear <- function(movies_with_keywords, year, n)
{
    
    if (is.null(year)) # no year given, so all the data (keywords_subset)
    { 
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords)[1:n,]
        ggplot(keyword_distribution_all, aes(x=keyword, y=count)) +
            geom_bar(stat='identity', fill = "#f3ce13") + 
            labs(title = "Keyword Distribution", x = "Keyword", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            ) + coord_flip()
    } 
    else # year given, need to compare year with overall
    { 
        year_films = movies_with_keywords[which(movies_with_keywords$year == year),]
        
        keyword_distribution_year =  distribution_of_keywords(year_films)[1:n,] %>% mutate(compare=year)
        
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords)[1:10,] %>%
            filter(keyword %in% keyword_distribution_year$keyword) %>% mutate(compare='All') 
        
        combined_data = rbind(keyword_distribution_year, keyword_distribution_all)
        
        ggplot(combined_data, aes(x=keyword, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Keyword Distribution", x = "Keyword", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            ) + coord_flip()
        
    }
}





########################## FUNCTIONS FOR OVERVIEW DECADE PLOTS #####################################
########################## FUNCTIONS FOR OVERVIEW DECADE PLOTS #####################################

plotFilmsByDecade = function(data, decade){
    decade_films = data %>% select(year) %>% 
        filter(year >= decade & year < decade + 10) %>% 
        summarise(count=n()) %>% 
        mutate(decade=decade)
    
    ggplot(decade_films, aes(x=as.factor(decade), y=count)) +
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste("Films in the ", decade, "'s", sep=''), x = "Decade", y = "Number of Films") +
        theme(text = element_text(size=20))
}


plotMonthPerGivenDecade <- function(data, decade)
{
    monthly_films_all = number_films_per_month(data)
    
    if (is.null(decade)) # no decade given, so all the data
    { 
        ggplot(monthly_films_all, aes(x=month, y=count)) +
            geom_bar(stat = "identity", fill = "#f3ce13") + 
            labs(title = "Monthly Distribution", x = "Month", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            )
    } 
    else # decade given, need to compare year with overall
    { 
        decade_films <- subset(data, year >= decade & year < decade + 10)
        
        monthly_films_decade = number_films_per_month(decade_films) %>% mutate(compare=decade)
        
        monthly_films_all = monthly_films_all %>% mutate(compare='All')
        
        combined_data = rbind(monthly_films_decade, monthly_films_all)
        
        ggplot(combined_data, aes(x=month, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Monthly Distribution", x = "Month", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            )
        
    }
}



plotRuntimePerGivenDecade <- function(data, decade)
{
    runtime_distribution_all = distribution_of_runtimes(data)
    
    if (is.null(decade)) # no decade given, so all the data
    { 
        ggplot(runtime_distribution_all, aes(x=runtime, y=count)) +
            geom_point() + labs(title = "Runtime Distribution", x = "Runtime", y = "Number of Films") +
            theme(text = element_text(size=20))
    } 
    else # decade given, need to compare decade with overall
    { 
        decade_films <- subset(data, year >= decade & year < decade + 10)
        
        runtime_distribution_decade =  distribution_of_runtimes(decade_films) %>% mutate(compare=decade)
        
        runtime_distribution_all = runtime_distribution_all %>% mutate(compare='All')
        
        combined_data = rbind(runtime_distribution_decade, runtime_distribution_all)
        
        ggplot(combined_data, aes(x=runtime, y=count, color=compare)) +
            geom_point() + labs(title = "Runtime Distribution", x = "Runtime", y = "Number of Films") +
            theme(text = element_text(size=20))
        
    }
    
}


plotGenrePerGivenDecade <- function(data, decade)
{
    genre_distribution_all = distribution_of_genres(data)
    
    if (is.null(decade)) # no decade given, so all the data
    { 
        ggplot(genre_distribution_all, aes(x=genre, y=count)) +
            geom_bar(stat='identity') + 
            labs(title = "Genre Distribution", x = "Genre", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
    } 
    else # decade given, need to compare decade with overall
    { 
        decade_films = subset(data, year >= decade & year < decade + 10)
        
        genre_distribution_decade =  distribution_of_genres(decade_films) %>% mutate(compare=decade)
        
        genre_distribution_all = genre_distribution_all %>% 
            filter(genre %in% genre_distribution_decade$genre) %>% 
            mutate(compare='All')
        
        combined_data = rbind(genre_distribution_decade, genre_distribution_all)
        
        ggplot(combined_data, aes(x=genre, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Genre Distribution", x = "Genre", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
        
    }
}




plotCertificatesPerGivenDecade <- function(data, decade)
{
    certificate_distribution_all = distribution_of_certificates(data)
    
    if (is.null(decade)) # no decade given, so all the data
    { 
        
        ggplot(certificate_distribution_all, aes(x=rating, y=count)) +
            geom_bar(stat='identity', fill = "#f3ce13") + 
            labs(title = "Certificate Distribution", x = "Certificate", y = "Number of Films") + coord_flip() + 
            theme(text = element_text(size=20))
    } 
    else # decade given, need to compare decade with overall
    { 
        decade_films = subset(data, year >= decade & year < decade + 10)
        
        certificate_distribution_decade =  distribution_of_certificates(decade_films) %>% mutate(compare=decade)
        
        certificate_distribution_all = certificate_distribution_all %>% 
            filter(rating %in% certificate_distribution_decade$rating) %>% 
            mutate(compare='All')
        
        combined_data = rbind(certificate_distribution_decade, certificate_distribution_all)
        
        ggplot(combined_data, aes(x=rating, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Certificate Distribution", x = "Certificate", y = "Number of Films") + coord_flip() +
            theme(text = element_text(size=20))
        
    }
}



plotTopKeywordsPerGivenDecade <- function(movies_with_keywords, decade, n)
{
    
    if (is.null(decade)) # no decade given, so all the data (keywords_subset)
    { 
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords)[1:n,]
        ggplot(keyword_distribution_all, aes(x=keyword, y=count)) +
            geom_bar(stat='identity', fill = "#f3ce13") + 
            labs(title = "Keyword Distribution", x = "Keyword", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            ) + coord_flip()
    } 
    else # decade given, need to compare year with overall
    { 
        decade_films = subset(movies_with_keywords, year >= decade & year < decade + 10)
        
        keyword_distribution_decade =  distribution_of_keywords(decade_films)[1:n,] %>% mutate(compare=decade)
        
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords)[1:n,] %>%
            filter(keyword %in% keyword_distribution_decade$keyword) %>% mutate(compare='All') 
        
        combined_data = rbind(keyword_distribution_decade, keyword_distribution_all)
        
        ggplot(combined_data, aes(x=keyword, y=count, fill=compare)) +
            geom_bar(stat='identity', position='dodge') + 
            labs(title = "Keyword Distribution", x = "Keyword", y = "Number of Films") +
            theme(text = element_text(size=20),
                  axis.text.x = element_text(angle = 45, hjust = 1)
            ) + coord_flip()
        
    }
}


########################## FUNCTIONS FOR GENRE PLOTS/TABLES #####################################
########################## FUNCTIONS FOR GENRE PLOTS/TABLES #####################################

plotYearByGenre <- function(data, genres)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    year_genres_graph = number_films_per_year(unique_movies_genre) %>% filter(year > 0)
    
    ggplot(year_genres_graph, aes(x=year, y=count)) + 
        geom_bar(stat = "identity", fill = "gray36") + 
        labs(title = paste(paste(genres, collapse='/'), "Films by Year", sep=" "), x = "Year", y = "Number of Films") +
        theme(text = element_text(size=20))
}

tableYearByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    number_films_per_year(unique_movies_genre)
}





plotDecadeByGenre <- function(data, genres)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    decade_genres_graph = number_films_per_decade(unique_movies_genre)
    
    ggplot(decade_genres_graph, aes(x=as.factor(decade), y=count)) + 
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste(paste(genres, collapse='/'), "Films by Decade", sep=" "), x = "Decade", y = "Number of Films") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
}


tableDecadeByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    number_films_per_decade(unique_movies_genre)
}




plotMonthByGenre <- function(data, genres)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    month_genres_graph = number_films_per_month(unique_movies_genre)
    
    ggplot(month_genres_graph, aes(x= month, y=count)) + 
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste(paste(genres, collapse='/'), "Films by Month", sep=" "), x = "Month", y = "Number of Films") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1)
        )
}


tableMonthByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    number_films_per_month(unique_movies_genre)
}



plotYearPercentageByGenre <- function(data, genres, by_year)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    year_genres_graph = number_films_per_year(unique_movies_genre) %>% arrange(year)
    
    year_genres_graph$total <- (by_year %>% filter(year %in% year_genres_graph$year) %>% arrange(year))$count 
    
    year_genres_graph = year_genres_graph %>% mutate(percent=count/total * 100)
    
    ggplot(year_genres_graph, aes(x= year, y=percent)) + 
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste("Percentage of", paste(genres, collapse='/'), "Films by Year", sep=" "), x = "Year", y = "Percentage") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1)
        )
}


tableYearPercentageByGenre <- function(data, genres, by_year)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    year_genres_graph = number_films_per_year(unique_movies_genre) %>% arrange(year)
    
    year_genres_graph$total <- (by_year %>% filter(year %in% year_genres_graph$year) %>% arrange(year))$count 
    
    year_genres_graph %>% mutate(percent=count/total * 100) %>% select(year, percent)
}



plotDecadePercentageByGenre <- function(data, genres, by_decade)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    decade_genres_graph = number_films_per_decade(unique_movies_genre) %>% arrange(decade)
    
    decade_genres_graph$total <- (by_decade %>% filter(decade %in% decade_genres_graph$decade) %>% arrange(decade))$count 
    
    decade_genres_graph = decade_genres_graph %>% mutate(percent=count/total * 100)
    
    ggplot(decade_genres_graph, aes(x=as.factor(decade), y=percent)) + 
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste("Percentage of", paste(genres, collapse='/'), "Films by Decade", sep=" "), x = "Decade", y = "Percentage") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1)
        )
}


tableDecadePercentageByGenre <- function(data, genres, by_decade)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    decade_genres_graph = number_films_per_decade(unique_movies_genre) %>% arrange(decade)
    
    decade_genres_graph$total <- (by_decade %>% filter(decade %in% decade_genres_graph$decade) %>% arrange(decade))$count 
    
    decade_genres_graph %>% mutate(percent=count/total * 100) %>% select(decade, percent)
}



plotMonthPercentageByGenre <- function(data, genres, by_month)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    month_genres_graph = number_films_per_month(unique_movies_genre) %>% arrange(month)
    
    month_genres_graph$total <- (by_month %>% filter(month %in% month_genres_graph$month) %>% arrange(month))$count 
    
    month_genres_graph = month_genres_graph %>% mutate(percent=count/total * 100)
    
    ggplot(month_genres_graph, aes(x= month, y=percent)) + 
        geom_bar(stat = "identity", fill = "#f3ce13") + 
        labs(title = paste("Percentage of", paste(genres, collapse='/'), "Films by Month", sep=" "), x = "Month", y = "Percentage") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1)
        )
}


tableMonthPercentageByGenre <- function(data, genres, by_month)
{
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    month_genres_graph = number_films_per_month(unique_movies_genre) %>% arrange(month)
    
    month_genres_graph$total <- (by_month %>% filter(month %in% month_genres_graph$month) %>% arrange(month))$count 
    
    month_genres_graph %>% mutate(percent=count/total * 100) %>% select(month, percent)
}


plotRuntimeByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    runtime_genres_graph = distribution_of_runtimes(unique_movies_genre)
    
    ggplot(runtime_genres_graph, aes(x=runtime, y=count)) + 
        geom_point() + 
        labs(title = paste(paste(genres, collapse='/'), "Films Runtime", sep=" "), x = "Runtime", y = "Number of Films") +
        theme(text = element_text(size=20))
    
}


tableRuntimeByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    distribution_of_runtimes(unique_movies_genre)
}



plotCertificatesByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    certificate_genres_graph = distribution_of_certificates(unique_movies_genre)
    
    ggplot(certificate_genres_graph, aes(x=rating, y=count)) + 
        geom_bar(stat='identity', fill= yellow) + 
        labs(title = paste(paste(genres, collapse='/'), "Film Certificates", sep=" "), x = "Certificate", y = "Number of Films") +
        theme(text = element_text(size=20)) + coord_flip()
    
}

tableCertificateByGenre <- function(data, genres)
{
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    distribution_of_certificates(unique_movies_genre)
}


plotTopKeywordsByGenre = function(data, genres, n){
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    keywords_genres_graph = distribution_of_keywords(unique_movies_genre)[1:n,]
    
    ggplot(keywords_genres_graph, aes(x=keyword, y=count)) + 
        geom_bar(stat='identity', fill= yellow) + 
        labs(title = paste(paste(genres, collapse='/'), "Film Keywords", sep=" "), x = "Keyword", y = "Number of Films") +
        theme(text = element_text(size=20)) + coord_flip()
}

tableTopKeywordsByGenre = function(data, genres, n){
    
    if ('All' %in% genres)
        unique_movies_genre = data
    else
        unique_movies_genre = data[which(data$genre %in% genres),]
    
    distribution_of_keywords(unique_movies_genre)[1:n,]
}

################################### ALL FOUR FILTERS ######################################

getMoviesFromFilter = function(data, keywords_subset, keywords, genres, runtime, certificates){
    
    keyword_in_keywords_movies = if ('All' %in% keywords)
        unique(keywords_subset$movie)
    else
        (keywords_subset %>% 
             select(movie, keyword) %>%
             filter(keyword %in% keywords))$movie %>%
        unique()
    
    genre_in_genres_movies = if ('All' %in% genres)
        unique(data$movie)
    else
        (data %>% 
             select(movie, genre) %>%
             filter(genre %in% genres))$movie %>% 
        unique()
    
    runtime_range = runtime[1]:runtime[2]
    
    runtime_in_range_movies = (data %>% 
                                   select(movie, runtime) %>% 
                                   filter(runtime %in% runtime_range))$movie %>% 
        unique()
    
    certificate_in_certificates_movies = if ('All' %in% certificates)
        unique(data$movie)
    else
        (data %>%
             select(movie, rating) %>% 
             filter(rating %in% certificates))$movie %>%
        unique()
    
    matching_movies = Reduce(intersect, 
                             list(keyword_in_keywords_movies, 
                                  genre_in_genres_movies, 
                                  runtime_in_range_movies,
                                  certificate_in_certificates_movies
                             )
    )
    
    data %>% filter(movie %in% matching_movies)
    
}













