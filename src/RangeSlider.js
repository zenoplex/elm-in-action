class RangeSlider extends HTMLElement {
  connectedCallback() {
    const input = document.createElement("input");
    globalThis.appendChild(input)
  }
}

globalThis.customElements.define("range-slider", RangeSlider);