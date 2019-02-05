#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Author: zhyh2010
# @Date: 2016-05-29 14:44:56
# @Last Modified by: anchen
# @Last Modified time: 2016-05-29 15:40:49
from flask import Flask, render_template, url_for, flash, redirect

from flask import request
from form import InputForm

app = Flask(__name__)
app.config['SECRET_KEY'] = '5791628bb0b13ce0c676dfde280ba245'
@app.route('/', methods=['GET', 'POST'])
def login():
    form =InputForm()
    if  request.method == "POST" and form.validate_on_submit():
       res = -41 + 0.9 * float(form.age.data) -  0.3 * float(form.waistline.data)
       res=round(res,1)
       return render_template('index.html',form=form,Result=res)

    #return render_template('index.html', title='body fat calculator', form=form)
    return render_template('index.html',form=form)
    #return render_template('main.html')
if __name__ == '__main__':
    app.run(debug=True)

