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

# function to get a count of movies from each decade as a table
number_films_per_decade = function(table) {

    # create empty dataframe of each decade in range from calculated min to max
    all_decades <- data.frame(formatC(1:((max_decade_all - min_decade_all) / 10 + 1), width = 2), 0)
    names(all_decades)[1] <- "decade"
    names(all_decades)[2] <- "count"
    all_decades$decade <- seq(from = min_decade_all, to = max_decade_all, by = 10)

    # INEFFICIENT NESTED LOOP, IDEALLY A GROUP_BY OR SOMETHING BETTER
    # loop through each movie in given table
    for (row in 1:as.numeric(count(table))) {

        # get the current movie's decade it belongs to
        curr_decade = floor(table$year[row] / 10) * 10

        # loop through each decade
        for (i in 1:as.numeric(count(all_decades))) {

            # increment the decade count for found decade
            if (curr_decade == all_decades$decade[i]) {
                all_decades$count[i] <- all_decades$count[i] + 1
                break
            }
        }
    }

    # output info
    total_films = as.numeric(sum(all_decades$count))
    print(paste0("---- total counted= ", total_films))
    unique_decades = as.numeric(count(all_decades))
    print(paste0("---- unique decades= ", unique_decades))

    # return the table of counts per decade
    all_decades
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

# function to get a distribution of top n keywords as a table
distribution_of_keywords = function(table, n) {

    # get a distribution of top n keywords
    by_keywords <- table %>% group_by(keyword) %>% summarize(count = n())

    # output info
    by_keywords <- top_n(by_keywords, n) %>% arrange(desc(count))
    print(paste0("---- total counted= ", sum(by_keywords$count)))
    unique_keywords = as.numeric(count(by_keywords))
    print(paste0("---- unique keywords= ", unique_keywords))

    # return the distribution of top n keywords
    by_keywords
}


########################## FUNCTIONS FOR PLOTS BELOW #####################################
########################## FUNCTIONS FOR PLOTS BELOW #####################################

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


plotGenrePerYear <- function(data, year)
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

plotCertificatesPerYear <- function(data, year)
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

plotTopKeywordsPerYear <- function(movies_with_keywords, year, n)
{
    
    if (is.null(year)) # no year given, so all the data (keywords_subset)
    { 
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords, n)
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
        
        keyword_distribution_year =  distribution_of_keywords(year_films, n) %>% mutate(compare=year)
        
        keyword_distribution_all = distribution_of_keywords(movies_with_keywords, nrow(movies_with_keywords)) %>%
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