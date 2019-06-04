package homework2

import java.time.LocalDate

import Validated.OptionToValidated

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError

case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError

case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError

case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError

case class YearIsNotAnInteger(year: String) extends DateError

case class MonthIsNotAnInteger(month: String) extends DateError

case class DayIsNotAnInteger(day: String) extends DateError

case class MonthOutOfRange(month: Int) extends DateError

case class DayOutOfRange(day: Int) extends DateError

case class InvalidDate(year: Int, month: Int, day: Int) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object UserRegistration {

  private def validateName(name: String): Validated[RegistrationFormError, String] =
    if (name.isEmpty) Invalid(NameIsEmpty)
    else Valid(name)

  private def validateEmail(email: String): Validated[RegistrationFormError, Email] = {
    val separated = email.split('@')
    if (separated.length != 2) Invalid(InvalidEmail(email))
    else Valid(Email(separated(0), separated(1)))
  }

  private def validatePasswordLength(password: String): Validated[RegistrationFormError, String] =
    if (password.length < 8) Invalid(PasswordTooShort)
    else Valid(password)

  private def validatePasswordSymbolVariety(password: String): Validated[RegistrationFormError, String] =
    if (password.matches("""^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}""")) Valid(password)
    else Invalid(PasswordRequiresGreaterSymbolVariety)

  private def validatePasswordsMatch(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] =
    if (password == passwordConfirmation) Valid(password)
    else Invalid(PasswordsDoNotMatch)

  private def validatePassword(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] =
    (validatePasswordLength(password),
      validatePasswordSymbolVariety(password),
      validatePasswordsMatch(password, passwordConfirmation)).zipMap((_, _, p) => PasswordUtils.hash(p))

  private def validateStringIsInt(str: String)(onError: => DateError): Validated[DateError, Int] = try {
    Valid(str.toInt)
  } catch {
    case _: NumberFormatException => Invalid(onError)
  }

  private def validateRange(n: Int)(f: Int => Boolean)(onError: => DateError): Validated[DateError, Int] =
    if (f(n)) Valid(n)
    else Invalid(onError)

  private def validateYear(year: String): Validated[DateError, Int] =
    validateStringIsInt(year)(YearIsNotAnInteger(year))

  private def validateMonth(month: String): Validated[DateError, Int] = for {
    intMonth <- validateStringIsInt(month)(MonthIsNotAnInteger(month))
    rangedMonth <- validateRange(intMonth)(m => m >= 1 && m <= 12)(MonthOutOfRange(intMonth))
  } yield rangedMonth

  private def validateDay(day: String): Validated[DateError, Int] = for {
    intDay <- validateStringIsInt(day)(DayIsNotAnInteger(day))
    rangedDay <- validateRange(intDay)(d => d >= 1 && d <= 31)(DayOutOfRange(intDay))
  } yield rangedDay

  private def validateBirthdayDateIsNotInTheFuture(date: Date, today: Date): Validated[RegistrationFormError, Date] =
    if (LocalDate.of(date.year, date.month, date.day).isAfter(LocalDate.of(today.year, today.month, today.day))) Invalid(BirthdayDateIsInTheFuture(date))
    else Valid(date)

  private def validateDate(yearStr: String, monthStr: String, dayStr: String)
                          (today: Date): Validated[RegistrationFormError, Date] = {
    val validatedInts = (validateYear(yearStr), validateMonth(monthStr), validateDay(dayStr)).zip match {
      case Invalid(errors) => Invalid(InvalidBirthdayDate(errors))
      case valid@Valid(_) => valid
    }

    for {
      ints <- validatedInts
      validDate <- Date.applyOption(ints._1, ints._2, ints._3).toValidated(InvalidBirthdayDate(Chain(InvalidDate(ints._1, ints._2, ints._3))))
      notInFutureDate <- validateBirthdayDateIsNotInTheFuture(validDate, today)
    } yield notInFutureDate
  }

  private def validatePostalCode(postalCodeVerifier: String => Boolean)
                                (postalCode: String): Validated[RegistrationFormError, Option[String]] =
    Valid(Some(postalCode).filter(postalCodeVerifier))

  def registerUser(userCountryPostalCodeVerifier: String => Boolean, today: Date)
                  (form: RegistrationForm): Validated[RegistrationFormError, User] = {
    (validateName(form.name),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation),
      validateDate(form.birthYear, form.birthMonth, form.birthDay)(today),
      validatePostalCode(userCountryPostalCodeVerifier)(form.postalCode),
    ).zipMap(User)
  }
}
