(function () {
  function repoBaseUrl() {
    const parts = window.location.pathname.split("/").filter(Boolean);
    // For https://user.github.io/<repo>/..., parts[0] is "<repo>"
    const repo = parts.length ? parts[0] : "";
    return window.location.origin + (repo ? `/${repo}/` : "/");
  }

  function findVersionNode() {
    // pkgdown typically has a navbar-text with the version
    const candidates = document.querySelectorAll(".navbar .navbar-text, .navbar span, .navbar a");
    for (const el of candidates) {
      const txt = (el.textContent || "").trim();
      if (/^v?\d+\.\d+\.\d+(\.\d+)?/.test(txt) || txt.toLowerCase().startsWith("version")) {
        return el;
      }
    }
    return null;
  }

  function buildDropdown(items, base) {
    // Bootstrap 5 dropdown markup
    const wrapper = document.createElement("div");
    wrapper.className = "dropdown";

    const btn = document.createElement("a");
    btn.className = "nav-link dropdown-toggle";
    btn.href = "#";
    btn.role = "button";
    btn.dataset.bsToggle = "dropdown";
    btn.ariaExpanded = "false";
    btn.textContent = "Versions";

    const menu = document.createElement("ul");
    menu.className = "dropdown-menu dropdown-menu-end";

    for (const it of items) {
      if (it === "---") {
        const li = document.createElement("li");
        li.innerHTML = '<hr class="dropdown-divider">';
        menu.appendChild(li);
        continue;
      }
      if (typeof it === "string") {
        const li = document.createElement("li");
        li.innerHTML = `<h6 class="dropdown-header">${it}</h6>`;
        menu.appendChild(li);
        continue;
      }
      const li = document.createElement("li");
      const a = document.createElement("a");
      a.className = "dropdown-item";
      a.href = it.url.startsWith("http") ? it.url : base + it.url.replace(/^\//, "");
      a.textContent = it.text;
      li.appendChild(a);
      menu.appendChild(li);
    }

    wrapper.appendChild(btn);
    wrapper.appendChild(menu);
    return wrapper;
  }

  async function run() {
    const base = repoBaseUrl();
    const url = base + "doc-versions.json";
    let items;
    try {
      const res = await fetch(url, { cache: "no-store" });
      if (!res.ok) return;
      items = await res.json();
    } catch (e) {
      return;
    }

    const versionNode = findVersionNode();
    if (!versionNode) return;

    // Put dropdown where the version text was (or near it)
    const parent = versionNode.parentElement;
    const dd = buildDropdown(items, base);

    // If versionNode is inside navbar, replace it; otherwise append to navbar
    if (parent) {
      parent.replaceChild(dd, versionNode);
    } else {
      const nav = document.querySelector(".navbar .navbar-nav");
      if (nav) nav.appendChild(dd);
    }
  }

  document.addEventListener("DOMContentLoaded", run);
})();

