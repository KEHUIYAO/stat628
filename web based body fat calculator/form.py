from flask_wtf import FlaskForm
from wtforms import StringField, PasswordField, SubmitField, RadioField
from wtforms.validators import DataRequired, Length, Email, EqualTo,NumberRange,ValidationError

def my_check(form, field):
    x=field.data
    try:
        x=float(x)
    except ValueError:
        raise ValidationError('Try to input numbers')

    if x <0 or x>300:
        raise ValidationError('Try to input a reasonable number')
class InputForm(FlaskForm):

    age = StringField('Abdomen (cm)',
                           validators=[DataRequired() ,my_check])
    waistline = StringField('Weight (lb)',
                        validators=[DataRequired(),my_check])

    submit = SubmitField('Calculate')

