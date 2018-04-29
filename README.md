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

### Then clone the repository, set the **`AHJ_HOME`** variable to source directory of project.

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

##### ONE LINE of bash script that builds the compiler and ONE LINE that runs the runtime.

To get list of **available actions**, run the following command:

        ahj -h

To **run** the language, simply compile the program as follows:
    
        ahj -f fileName.ahj

---

## **System Information**

The language's **compiler** and **runtime** is built for **_Linux, Windows, and Mac OS X_** platforms.
The procedure of *tokenizing, parsing,* and *intepretting* the syntax is entirely done through Prolog.

[Josh Hewlett] Bash script does...

---
## **Tools Used:**
### The following tools were used:
* **Prolog**
* **Bash Scripts**
* **DCG Grammar Rules** via Prolog

---
### **YouTube video**
#### **Project overview**
[![FullPortalTutorial](https://img.youtube.com/vi/Z5AmqMuNi08/0.jpg)](https://www.youtube.com/watch?v=Z5AmqMuNi08)

---
## **Authors**
*  [github/Alireza Bahremand](https://github.com/TheWiselyBearded)
* [github/Cecilia LaPlace](https://github.com/HalcyonAura)
* [github/Josh Hewlett](https://github.com/joshhewlett)
* [github/Paul Horton](https://github.com/PaHorton)

### **License**

Copyright © 2018,
Released under the [MIT license](LICENSE).