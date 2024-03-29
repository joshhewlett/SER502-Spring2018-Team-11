# ***AH-J Programming Language***
## SER-502 Group 11
---
## **Installing Language**

### First, install SWI-PL on your local machine:
* #### [Windows Download](http://www.swi-prolog.org/download/stable "Prolog Windows Download")
* Mac OS (using [homebrew]): ```brew install swi-prolog```

[homebrew]: http://brew.sh/
* Linux: Follow [the instructions][linux-setup] from the official website. Run the
Prolog shell from the command line with `swipl`.

[linux-setup]: http://www.swi-prolog.org/build/unix.html

Then clone the repository, set the **`AHJ_HOME`** variable to `/src` directory of project.

    $ export AHJ_HOME=[repo root]/src

---
## **Building & Running Language**

### Syntax Guidelines:
* #### [Language BNF](https://drive.google.com/open?id=1qJhU3IedlJXfJBMdjmeciZGR_OXEmlq-dDAnwkuDke8 ("AH-J BCF"))
### Example Program Excerpt:
```
Salutations Xiangyu, 
    Would you mind doing the following:

        Create the variable bar.
        Create the variable x.
        Assign the integer foo to the value of 8 / 4.
        Assign the boolean bar to the value of 0.

        Should it be the case bar EQUALS 0 please 
            Would you mind doing the following:
                Assign the integer x to the value of 4.
                Please reply with the value of x.
            Thank you.
        otherwise
            Would you mind doing the following:
                Please reply with the value of foo. 
            Thank you. 
        that is all.

    Thank you. 
Sincerely, Ajay Bansal
```


### **Compiler/Runtime:**


To get list of **available actions**, run the following command:

        $ $AHJ_HOME/ahj.sh -h

To **run** the language, simply compile the program as follows:
    
        $ $AHJ_HOME/ahj.sh -f fileName.ahj

---

## **System Information**

The language's **compiler** and **runtime** is built for **_Linux, and Mac OS X_** platforms.
The procedure of *tokenizing, parsing,* and *intepreting* the syntax is entirely done through Prolog.

The main `ahj.sh` bash script converts the program to a copy of it with spaces between any words and periods. This is then fed directly into the main `parser.pl` prolog program that tokenizes, parses and interprets the program. 

---
## **Tools Used:**
As a whole, **Prolog** is a versatile program, and therefore was the focal point of all components of the project. Using prolog allowed for the ability to tokenize, parse, and interpret custom code-as described above. 

Using Prolog as a _tokenizer_ enabled the creation of a list that houses the breakdown of the AH-J code. This was reliant upon previously identified tokens. This generates the next functionality that Prolog supports.

Using Prolog as a _parser_ allowed for code checking through its DCG functionality. This turns the previously tokenized list into a labeled structure. This allows for syntax checking. It also leads into the final use of Prolog in this project: the interpreter.

Using Prolog as an _interpreter_ completed the development of a language by giving it semantics. In doing so, it evaluates the progra and provides semantic checking.
Finally, the _bash script_ executes our code ensemble to promote abstraction but also step by step texual visualization of the compilation process. 

---
### **YouTube video**
#### **Project overview**
[![FullPortalTutorial](https://img.youtube.com/vi/RKvrfUexI3I/0.jpg)](https://www.youtube.com/watch?v=RKvrfUexI3I)

---
## **Authors**
*  [github/Alireza Bahremand](https://github.com/TheWiselyBearded)
* [github/Cecilia LaPlace](https://github.com/HalcyonAura)
* [github/Josh Hewlett](https://github.com/joshhewlett)
* [github/Paul Horton](https://github.com/PaHorton)

### **License**

Copyright © 2018,
Released under the MIT license.
