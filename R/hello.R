library(roxygen2)
library(devtools)

##I included mode because I think it's weird that base R doesn't

#'mode
#'
#' @description This function returns the mode of the data passed to it
#'
#' @param data A vector or factor that you wish to find the mode of
#' @return The same data type as the data you passed it. The Mode of that data
#' @examples
#' mode(c("Alex","Ben","Alex","Alex",",Ben",",Tim",",Tom"))
#' @export
mode <- function(data){
  table <- table(data)
  most_frequent_value <- names(table)[table == max(table)]
  return(most_frequent_value)
}

#'anti_mode
#'
#' @description This function returns the anti_mode(Least frequent element) of the data passed to it
#'
#' @param data A vector or factor that you wish to find the anti_mode of
#' @return The same data type as the data you passed it. The Anti_Mode of that data
#' @examples
#' anti_mode(c("Alex","Ben","Alex","Alex",",Ben",",Tim",",Tom"))
#' @export
anti_mode <- function(data){
  table <- table(data)
  most_frequent_value <- names(table)[table == min(table)]
  return(most_frequent_value)
}


#Source for the names https://www.whattoexpect.com/baby-names/list/top-baby-names-for-boys/
top_boy_names <- c("Liam",
                            "Noah",
                            "Oliver",
                            "James",
                            "Elijah",
                            "Mateo",
                            "Theodore",
                            "Henry",
                            "Lucas",
                            "William",
                            "Benjamin",
                            "Levi",
                            "Sebastian",
                            "Jack",
                            "Ezra",
                            "Michael",
                            "Daniel",
                            "Leo",
                            "Owen",
                            "Samuel",
                            "Hudson",
                            "Alexander",
                            "Asher",
                            "Luca",
                            "Ethan",
                            "John",
                            "David",
                            "Jackson",
                            "Joseph",
                            "Mason",
                            "Luke",
                            "Matthew",
                            "Julian",
                            "Dylan",
                            "Elias",
                            "Jacob",
                            "Maverick",
                            "Gabriel",
                            "Logan",
                            "Aiden",
                            "Thomas",
                            "Isaac",
                            "Miles",
                            "Grayson",
                            "Santiago",
                            "Anthony",
                            "Wyatt",
                            "Carter",
                            "Jayden",
                            "Ezekiel",
                            "Caleb",
                            "Cooper",
                            "Josiah",
                            "Charles",
                            "Christopher",
                            "Isaiah",
                            "Nolan",
                            "Cameron",
                            "Nathan",
                            "Joshua",
                            "Kai",
                            "Waylon",
                            "Angel",
                            "Lincoln",
                            "Andrew",
                            "Roman",
                            "Adrian",
                            "Aaron",
                            "Wesley",
                            "Ian",
                            "Thiago",
                            "Axel",
                            "Brooks",
                            "Bennett",
                            "Weston",
                            "Rowan",
                            "Christian",
                            "Theo",
                            "Beau",
                            "Eli",
                            "Silas",
                            "Jonathan",
                            "Ryan",
                            "Leonardo",
                            "Walker",
                            "Jaxon",
                            "Micah",
                            "Everett",
                            "Robert",
                            "Enzo",
                            "Parker",
                            "Jeremiah",
                            "Jose",
                            "Colton",
                            "Luka",
                            "Easton",
                            "Landon",
                            "Jordan",
                            "Amir",
                            "Gael",
                            "Austin",
                            "Adam",
                            "Jameson",
                            "Aug")


#Source for the names https://www.whattoexpect.com/baby-names/list/top-baby-names-for-girls/
#I chose the Top 150 names
top_girl_names <- c("Olivia",
                               "Emma",
                               "Charlotte",
                               "Amelia",
                               "Sophia",
                               "Mia",
                               "Isabella",
                               "Ava",
                               "Evelyn",
                               "Luna",
                               "Harper",
                               "Sofia",
                               "Camila",
                               "Eleanor",
                               "Elizabeth",
                               "Violet",
                               "Scarlett",
                               "Emily",
                               "Hazel",
                               "Lily",
                               "Gianna",
                               "Aurora",
                               "Penelope",
                               "Aria",
                               "Nora",
                               "Chloe",
                               "Ellie",
                               "Mila",
                               "Avery",
                               "Layla",
                               "Abigail",
                               "Ella",
                               "Isla",
                               "Eliana",
                               "Nova",
                               "Madison",
                               "Zoe",
                               "Ivy",
                               "Grace",
                               "Lucy",
                               "Willow",
                               "Emilia",
                               "Riley",
                               "Naomi",
                               "Victoria",
                               "Stella",
                               "Elena",
                               "Hannah",
                               "Valentina",
                               "Maya",
                               "Zoey",
                               "Delilah",
                               "Leah",
                               "Lainey",
                               "Lillian",
                               "Paisley",
                               "Genesis",
                               "Madelyn",
                               "Sadie",
                               "Sophie",
                               "Leilani",
                               "Addison",
                               "Natalie",
                               "Josephine",
                               "Alice",
                               "Ruby",
                               "Claire",
                               "Kinsley",
                               "Everly",
                               "Emery",
                               "Adeline",
                               "Kennedy",
                               "Maeve",
                               "Audrey",
                               "Autumn",
                               "Athena",
                               "Eden",
                               "Iris",
                               "Anna",
                               "Eloise",
                               "Jade",
                               "Maria",
                               "Caroline",
                               "Brooklyn",
                               "Quinn",
                               "Aaliyah",
                               "Vivian",
                               "Liliana",
                               "Gabriella",
                               "Hailey",
                               "Sarah",
                               "Savannah",
                               "Cora",
                               "Madeline",
                               "Natalia",
                               "Ariana",
                               "Lydia",
                               "Lyla",
                               "Clara",
                               "Allison",
                               "Aubrey",
                               "Millie",
                               "Melody",
                               "Ayla",
                               "Serenity",
                               "Bella",
                               "Skylar",
                               "Josie",
                               "Lucia",
                               "Daisy",
                               "Raelynn",
                               "Eva",
                               "Juniper",
                               "Samantha",
                               "Elliana",
                               "Eliza",
                               "Rylee",
                               "Nevaeh",
                               "Hadley",
                               "Alaia",
                               "Parker",
                               "Julia",
                               "Amara",
                               "Rose",
                               "Charlie",
                               "Ashley",
                               "Remi",
                               "Georgia",
                               "Adalynn",
                               "Melanie",
                               "Amira",
                               "Margaret",
                               "Piper",
                               "Brielle",
                               "Mary",
                               "Freya",
                               "Cecilia",
                               "Esther",
                               "Arya",
                               "Sienna",
                               "Summer",
                               "Peyton",
                               "Sage",
                               "Valerie",
                               "Magnolia",
                               "Emersyn",
                               "Catalina",
                               "Margot",
                               "Everleigh",
                               "Alina")

#Source for the surnames https://names.mongabay.com/most_common_surnames.htm
top_surnames <- c("Johnson",
                  "Williams",
                  "Jones",
                  "Brown",
                  "Davis",
                  "Miller",
                  "Wilson",
                  "Moore",
                  "Taylor",
                  "Anderson",
                  "Thomas",
                  "Jackson",
                  "White",
                  "Harris",
                  "Martin",
                  "Thompson",
                  "Garcia",
                  "Martinez",
                  "Robinson",
                  "Clark",
                  "Rodriguez",
                  "Lewis",
                  "Lee",
                  "Walker",
                  "Hall",
                  "Allen",
                  "Young",
                  "Hernandez",
                  "King",
                  "Wright",
                  "Lopez",
                  "Hill",
                  "Scott",
                  "Green",
                  "Adams",
                  "Baker",
                  "Gonzalez",
                  "Nelson",
                  "Carter",
                  "Mitchell",
                  "Perez",
                  "Roberts",
                  "Turner",
                  "Phillips",
                  "Campbell",
                  "Parker",
                  "Evans",
                  "Edwards",
                  "Collins",
                  "Stewart",
                  "Sanchez",
                  "Morris",
                  "Rogers",
                  "Reed",
                  "Cook",
                  "Morgan",
                  "Bell",
                  "Murphy",
                  "Bailey",
                  "Rivera",
                  "Cooper",
                  "Richardson",
                  "Cox",
                  "Howard",
                  "Ward",
                  "Torres",
                  "Peterson",
                  "Gray",
                  "Ramirez",
                  "James",
                  "Watson",
                  "Brooks",
                  "Kelly",
                  "Sanders",
                  "Price",
                  "Bennett",
                  "Wood",
                  "Barnes",
                  "Ross",
                  "Henderson",
                  "Coleman",
                  "Jenkins",
                  "Perry",
                  "Powell",
                  "Long",
                  "Patterson",
                  "Hughes",
                  "Flores",
                  "Washington",
                  "Butler",
                  "Simmons",
                  "Foster",
                  "Gonzales",
                  "Bryant",
                  "Alexander",
                  "Russell",
                  "Griffin",
                  "Diaz",
                  "Hayes",
                  "Myers",
                  "Ford",
                  "Hamilton",
                  "Graham",
                  "Sullivan",
                  "Wallace",
                  "Woods",
                  "Cole",
                  "West",
                  "Jordan",
                  "Owens",
                  "Reynolds",
                  "Fisher",
                  "Ellis",
                  "Harrison",
                  "Gibson",
                  "Mcdonald",
                  "Cruz",
                  "Marshall",
                  "Ortiz",
                  "Gomez",
                  "Murray",
                  "Freeman",
                  "Wells",
                  "Webb",
                  "Simpson",
                  "Stevens",
                  "Tucker",
                  "Porter",
                  "Hunter",
                  "Hicks",
                  "Crawford",
                  "Henry",
                  "Boyd",
                  "Mason",
                  "Morales",
                  "Kennedy",
                  "Warren",
                  "Dixon",
                  "Ramos",
                  "Reyes",
                  "Burns",
                  "Gordon",
                  "Shaw",
                  "Holmes",
                  "Rice",
                  "Robertson",
                  "Hunt",
                  "Black",
                  "Daniels",
                  "Palmer",
                  "Mills",
                  "Nichols",
                  "Grant",
                  "Knight",
                  "Ferguson",
                  "Rose",
                  "Stone",
                  "Hawkins",
                  "Dunn",
                  "Perkins",
                  "Hudson",
                  "Spencer",
                  "Gardner",
                  "Stephens",
                  "Payne",
                  "Pierce",
                  "Berry",
                  "Matthews",
                  "Arnold",
                  "Wagner",
                  "Willis",
                  "Ray",
                  "Watkins",
                  "Olson",
                  "Carroll",
                  "Duncan",
                  "Snyder",
                  "Hart",
                  "Cunningham",
                  "Bradley",
                  "Lane",
                  "Andrews",
                  "Ruiz",
                  "Harper",
                  "Fox",
                  "Riley",
                  "Armstrong",
                  "Carpenter",
                  "Weaver",
                  "Greene",
                  "Lawrence",
                  "Elliott",
                  "Chavez",
                  "Sims",
                  "Austin",
                  "Peters",
                  "Kelley",
                  "Franklin",
                  "Lawson",
                  "Fields",
                  "Gutierrez",
                  "Ryan",
                  "Schmidt",
                  "Carr",
                  "Vasquez",
                  "Castillo",
                  "Wheeler",
                  "Chapman",
                  "Oliver",
                  "Montgomery",
                  "Richards",
                  "Williamson",
                  "Johnston",
                  "Banks",
                  "Meyer",
                  "Bishop",
                  "Mccoy",
                  "Howell",
                  "Alvarez",
                  "Morrison",
                  "Hansen",
                  "Fernandez",
                  "Garza",
                  "Harvey",
                  "Little",
                  "Burton",
                  "Stanley",
                  "Nguyen",
                  "George",
                  "Jacobs",
                  "Reid",
                  "Kim",
                  "Fuller",
                  "Lynch",
                  "Dean",
                  "Gilbert",
                  "Garrett",
                  "Romero",
                  "Welch",
                  "Larson",
                  "Frazier",
                  "Burke",
                  "Hanson",
                  "Day",
                  "Mendoza",
                  "Moreno",
                  "Bowman",
                  "Medina",
                  "Fowler",
                  "Brewer",
                  "Hoffman",
                  "Carlson",
                  "Silva",
                  "Pearson",
                  "Holland",
                  "Douglas",
                  "Fleming",
                  "Jensen",
                  "Vargas",
                  "Byrd",
                  "Davidson",
                  "Hopkins",
                  "May",
                  "Terry",
                  "Herrera",
                  "Wade",
                  "Soto",
                  "Walters",
                  "Curtis",
                  "Neal",
                  "Caldwell",
                  "Lowe",
                  "Jennings",
                  "Barnett",
                  "Graves",
                  "Jimenez",
                  "Horton",
                  "Shelton",
                  "Barrett",
                  "Obrien",
                  "Castro",
                  "Sutton",
                  "Gregory",
                  "Mckinney",
                  "Lucas",
                  "Miles",
                  "Craig",
                  "Rodriquez",
                  "Chambers",
                  "Holt",
                  "Lambert",
                  "Fletcher",
                  "Watts",
                  "Bates",
                  "Hale",
                  "Rhodes",
                  "Pena",
                  "Beck",
                  "Newman")


#' gen_boy_name
#'
#' This function generates a random boy name
#'
#' @return String. Represents a name for a boy
#' @examples
#' # Generate a random boy's name
#' gen_boy_name()
#' @export
gen_boy_name <- function(){
  return(sample(top_boy_names,1))
}

#' gen_girl_name
#'
#' This function generates a random girl name
#'
#' @return String. Represents a name for a girl
#' @examples
#' # Generate a random girl's name
#' gen_girl_name()
#' @export
gen_girl_name <- function(){
  return(sample(top_girl_names,1))
}


#' gen_surname
#'
#' This function generates a random surname
#'
#' @return String. Represents a common surname
#' @examples
#' # Generate a random surname
#' gen_surname()
#' @export
gen_surname <- function(){
  return(sample(top_surnames,1))
}

email_hosts <- c("gmail","yahoo","gmx","outlook","icloud","aol","yandex","mail","protonmail")

#' gen_email_address
#'
#' This function generates a fake email address using a first name and surname.
#'
#'
#' @param firstname A String representing the first name of the person you want to generate an email for
#' @param surname A String representing the surname of the person you want to generate an email for
#' @return String. Represents an Email Address
#' @examples
#' # Generate an email address for Joe Miller
#' gen_email_address("Joe","Miller")
#' @export
gen_email_address <- function(firstname,surname){
  return(paste0(firstname,".", surname,"@",sample(email_hosts,1),".com"))
}

#' gen_birthday
#'
#' This function generates a random date that is at least 18 years ago and at max 65 yeas ago
#'
#' @return Date. A randomly selected Birthday
#' @examples
#' gen_birthday()
#' @export
gen_birthday <- function(){
  current_date <- Sys.Date()

  #18 Years ago is the maximum
  max_date <- as.Date(format(current_date, "%Y-%m-%d")) - 18 * 365

  #65 years ago is the minimum
  min_date <- as.Date(format(current_date, "%Y-%m-%d")) - 65 * 365

  #Calculate the total amount of days between 18 and 65 years ago
  total_days <- as.numeric(difftime(max_date, min_date, units = "days"))

  #Add a random number of days in between 0 and the total amount of days to the minimum date
  date <- min_date + sample(0:total_days,1)
  return(date)
}


#' gen_annual_income
#'
#' This function generates a random annual income that is at least 10000 and at most 620000 and always divisible by 500
#'
#' @return Numeric. The annual income
#' @examples
#' gen_annual_income()
#' @export
gen_annual_income <- function(){
  sample(seq(10000,620000,by=500),1)
}

sexes <- c("male","female")

#' gen_sex
#'
#' This function generates a random sex(male or female)
#'
#' @return String. It represents a sex("male" or "female")
#' @examples
#' gen_sex()
#' @export
gen_sex <- function(){
  sample(sexes,1)
}


#' gen_person
#'
#' This function generates a random person
#'
#'
#' @return Data frame. It represents a person with the columns Name,Surname,Sex,Email,Birthday and, Annual income
#' @examples
#' # Generate a random person
#' gen_person()
#' @export
gen_person <- function(){
  sex <- gen_sex()
  surname <- gen_surname()

  if(sex =="male"){
    name <- gen_boy_name()
    person <- data.frame(
    Name = name,
    Surname = surname,
    Sex = sex,
    Email = gen_email_address(name,surname),
    Birthday = gen_birthday(),
    Annual_Income = gen_annual_income()
  )

  return(person)
  }

  name <- gen_girl_name()
  person <- data.frame(
    Name = name,
    Surname = surname,
    Sex = sex,
    Email = gen_email_address(name,surname),
    Birthday = gen_birthday(),
    Annual_Income = gen_annual_income()
    )
  return(person)
}


#' gen_people
#'
#' This function generates a data frame with information about lots of people
#'
#'
#' @param amount The amount of people you want to generate
#' @return Data Frame. A data frame with the specified amount of rows, each representing a person with the columns Name, Surname, Sex, Email, Birthday, and Annual_Income
#' @examples
#' # Generate information for 151 people
#' gen_people(151)
#' @export
gen_people <- function(amount){
  people <- lapply(1:amount, function(i) gen_person())
  return(do.call(rbind, people))
}




