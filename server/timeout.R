timeoutSeconds <- 300
warnSeconds <- 240

inactivity <- sprintf("function idleTimer() {
        
        var t = setTimeout(logout, %s);
        
        var t2 = setTimeout(warn, %s );
        
        var a = 1;

        window.onmousemove = resetTimer; // catches mouse movements
        window.onmousedown = resetTimer; // catches mouse movements
        window.onclick = resetTimer;     // catches mouse clicks
        window.onscroll = resetTimer;    // catches scrolling
        window.onkeypress = resetTimer;  //catches keyboard actions

        function logout() {
            Shiny.setInputValue('timeOut', '%s')
        }

        function warn() {
            Shiny.setInputValue('warnTimeOut', a)
            a++;
        }

        function resetTimer() {
            clearTimeout(t);
            clearTimeout(t2);
            t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
            t2 = setTimeout(warn, %s);  // time is in milliseconds (1000 is 1 second)
        }
    }
    idleTimer();", 
    timeoutSeconds*1000, 
    warnSeconds*1000, 
    timeoutSeconds, 
    timeoutSeconds*1000, 
    warnSeconds*1000
    # Each of the '%s' corresponds to the a statement in the comma delim string. 
)

