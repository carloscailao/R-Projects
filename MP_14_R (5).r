# ********************
# Last names: Cailao, Colobong, Domanais, Kaw
# Language: R
# Paradigm(s): Functional, Imperative, Procedural
# ********************

get_numeric_input <- function(prompt_text, allow_zero = TRUE, allow_fraction = TRUE) {
  repeat {
    # initial input
    input_raw <- readline(prompt = prompt_text)
    
    # parse and remove comma and percent
    input_cleaned <- gsub("[,%]", "", input_raw)
    
    # supress R warning because we already deal with invalid inputs
    # convert as numeric the cleaned input
    input <- suppressWarnings(as.numeric(input_cleaned))
    
    # if inputs passes our checks, return, else print error message
    if (!is.na(input) &&
        input >= 0 &&
        input <= 1e10 &&
        (allow_zero || input != 0) &&
        (allow_fraction || input == floor(input))) {
      return(input)
    } else {
      if (is.na(input)) {
        cat("Invalid input. Please enter a numeric value.\n")
      } else if (!allow_zero && input == 0) {
        cat("Invalid input. Zero is not allowed for this field.\n")
      } else if (!allow_fraction && input != floor(input)) {
        cat("Invalid input. Please enter a whole number.\n")
      } else if (input < 0 ){
        cat("Invalid input. Negative values are not allowed.")
      } else if (input > 1e10) {
        cat("Invalid input. The value is too large. Please enter a reasonable value.\n")
      } else {
        cat("Invalid input. Please enter a valid numeric value.\n")
      }
    }
  }
}

loan_calculator <- function() {
  # user input with handling for zeroes or fractions
  loan_amount <- get_numeric_input("Enter Loan Amount in PHP: ", allow_zero = FALSE, allow_fraction = TRUE)
  annual_interest_rate <- get_numeric_input("Enter Annual Interest Rate in %: ", allow_zero = TRUE, allow_fraction = TRUE)
  loan_term_years <- get_numeric_input("Enter Loan Term in Years: ", allow_zero = FALSE, allow_fraction = TRUE)
  
  monthly_interest_rate <- annual_interest_rate / 12 / 100
  
  loan_term_months <- loan_term_years * 12
  
  total_interest <- loan_amount * monthly_interest_rate * loan_term_months
  
  monthly_repayment <- (loan_amount + total_interest) / loan_term_months
  
  # format parameter 'big.mark' separates thousands by commas
  # format parameter 'round' rounds off the value to 2 decimal places
  # scientific = false prevents displaying in scientific notation
  # cat is concatenate and print
  cat("\nLoan Details:\n")
  cat("Loan Amount: PHP", format(loan_amount, big.mark = ",", scientific = FALSE), "\n")
  cat("Annual Interest Rate:", annual_interest_rate, "%\n")
  cat("Loan Term:", loan_term_months, "months\n")
  cat("Monthly Repayment: PHP", format(round(monthly_repayment, 2), big.mark = ",", scientific = FALSE), "\n")
  cat("Total Interest: PHP", format(round(total_interest, 2), big.mark = ",", scientific = FALSE), "\n")
}

# run
loan_calculator()
