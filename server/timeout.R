timeoutSeconds <- 600
warnSeconds <- 540
last_activity <- Sys.time()
idle_warning_shown <- FALSE

observeEvent(input$analyzer_activity, {
    last_activity <<- Sys.time()
    if (idle_warning_shown) {
        removeModal()
        idle_warning_shown <<- FALSE
    }
}, ignoreInit = FALSE)

observeEvent(input$analyzer_continue_session, {
    last_activity <<- Sys.time()
    idle_warning_shown <<- FALSE
    removeModal()
})

observe({
    invalidateLater(5000, session)
    idle_seconds <- as.numeric(difftime(Sys.time(), last_activity, units = "secs"))

    if (idle_seconds >= timeoutSeconds) {
        showModal(modalDialog(
            title = "Session expired",
            "Your session ended after 10 minutes of inactivity. Reload to start a new session. Uploaded data and unsaved analysis state will be lost.",
            easyClose = FALSE,
            footer = tags$button(
                type = "button", class = "btn btn-primary",
                onclick = "window.location.reload();", "Reload"
            )
        ))
        session$onFlushed(function() session$close(), once = TRUE)
    } else if (idle_seconds >= warnSeconds && !idle_warning_shown) {
        idle_warning_shown <<- TRUE
        showModal(modalDialog(
            title = "Session expiring",
            "Your session will close in about one minute due to inactivity. Continue to keep your current work.",
            easyClose = FALSE,
            footer = actionButton("analyzer_continue_session", "Continue Session")
        ))
    }
})

inactivity <- "(function() {
    if (window.pvaIdleActivityInstalled) return;
    window.pvaIdleActivityInstalled = true;
    let lastActivitySent = 0;
    let connected = true;

    function reportActivity() {
        if (!connected || document.hidden) return;
        const now = Date.now();
        if (now - lastActivitySent < 10000) return;
        lastActivitySent = now;
        Shiny.setInputValue('analyzer_activity', now, {priority: 'event'});
    }

    $(document).on('shiny:connected.pvaIdle', function() {
        connected = true;
        lastActivitySent = 0;
        reportActivity();
    });
    $(document).on('shiny:disconnected.pvaIdle', function() {
        connected = false;
    });
    ['pointerdown', 'keydown', 'input', 'scroll', 'touchstart'].forEach(function(eventName) {
        document.addEventListener(eventName, reportActivity, {passive: true, capture: true});
    });
    reportActivity();
})();"