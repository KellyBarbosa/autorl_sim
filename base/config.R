css <- "
    .reduce-margin {
      margin-top: 0;
    }

    a {
      text-decoration: none !important;
    }

    .limit_sidebarPanel {
      width: 250px;
    }

    .increase-margin {
      margin-top: 15px;
    }

    .image-container {
      position: relative;
      margin: 0 360px 0 315px;
    }

    .button-container {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      display: none;
    }

    .image-container:hover .button-container {
      display: block;
    }
    "

js <- "window.onbeforeunload = function() {
      Shiny.setInputValue('tab_closed', true, {priority: 'event'});
    };" 