/* ============================================================
   dashboard.js  -  Crime Analytics Platform v2.1
   Fixes all 4 reported issues:
     1. Active nav highlight synchronized with page switches
     2. Scroll-to-top on every page switch
     3. KPI entrance animation
   ============================================================ */
(function () {
  "use strict";

  /* -- setActiveNav: match by data-nav-id (not element id)
        so it never conflicts with Shiny's input binding system -- */
  function setActiveNav(tabId) {
    document.querySelectorAll(".nav-btn").forEach(function (btn) {
      btn.classList.remove("active");
    });
    /* Primary: match by data-nav-id attribute */
    var target = document.querySelector('[data-nav-id="' + tabId + '"]');
    /* Fallback: match by element id (old HTML may still have id=) */
    if (!target) { target = document.getElementById(tabId); }
    if (target) { target.classList.add("active"); }
  }

  /* -- Scroll the .main panel back to the top --------------- */
  function scrollMain() {
    var main = document.querySelector(".main");
    if (main) { main.scrollTop = 0; }
  }

  /* -- Re-run KPI animation after page switch --------------- */
  function animateKPIs() {
    var cards = document.querySelectorAll(".kpi-card");
    cards.forEach(function (card, i) {
      card.style.opacity   = "0";
      card.style.transform = "translateY(12px)";
      card.style.transition =
        "opacity 0.32s ease " + (i * 0.07) + "s," +
        "transform 0.32s ease " + (i * 0.07) + "s";
      /* Two rAF frames: first sets initial state, second triggers transition */
      requestAnimationFrame(function () {
        requestAnimationFrame(function () {
          card.style.opacity   = "1";
          card.style.transform = "translateY(0)";
        });
      });
    });
  }

  /* -- DOM ready: set initial active state ------------------ */
  document.addEventListener("DOMContentLoaded", function () {
    setActiveNav("nav_overview");
    setTimeout(animateKPIs, 250);
  });

  /* -- Shiny input changed: fires BEFORE renderUI completes
        so we set nav highlight immediately, then scroll after
        a small delay to let the DOM settle ------------------- */
  $(document).on("shiny:inputchanged", function (e) {
    if (e.name === "active_tab") {
      /* Highlight fires immediately on click */
      setActiveNav(e.value);
      /* Scroll and animate after renderUI re-draws the page (~80ms) */
      setTimeout(function () {
        scrollMain();
        animateKPIs();
      }, 80);
    }
  });

  /* -- page_body re-render: catch any missed scroll/animate -- */
  $(document).on("shiny:value", function (e) {
    if (e.name === "page_body") {
      setTimeout(function () {
        scrollMain();
        animateKPIs();
      }, 120);
    }
  });

  /* -- Custom message handlers ------------------------------- */
  Shiny.addCustomMessageHandler("setActiveNav", function (id) {
    setActiveNav(id);
    scrollMain();
  });

  /* toast notification */
  Shiny.addCustomMessageHandler("toast", function (msg) {
    var toast = document.createElement("div");
    toast.style.cssText =
      "position:fixed;bottom:24px;right:24px;z-index:9999;" +
      "background:#161b22;border:1px solid #30363d;border-radius:8px;" +
      "padding:12px 18px;color:#e6edf3;font-size:13px;" +
      "box-shadow:0 8px 24px rgba(0,0,0,.5);" +
      "opacity:0;transform:translateY(12px);" +
      "transition:opacity .3s ease,transform .3s ease;";
    toast.textContent = msg.text || "";
    document.body.appendChild(toast);
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        toast.style.opacity   = "1";
        toast.style.transform = "translateY(0)";
      });
    });
    setTimeout(function () {
      toast.style.opacity   = "0";
      toast.style.transform = "translateY(12px)";
      setTimeout(function () {
        if (toast.parentNode) { toast.parentNode.removeChild(toast); }
      }, 350);
    }, (msg.duration || 4) * 1000);
  });

  /* -- Plotly resize on window resize ----------------------- */
  window.addEventListener("resize", function () {
    document.querySelectorAll(".plotly-graph-div").forEach(function (div) {
      if (window.Plotly) { Plotly.Plots.resize(div); }
    });
  });

}());
