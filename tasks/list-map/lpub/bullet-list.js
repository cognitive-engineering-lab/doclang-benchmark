import { DependentElement } from "@living-papers/components";

export default class BulletList extends DependentElement {
  static get properties() {
    return {
      items: { type: Array },
    };
  }

  constructor() {
    super();
    this.items = [];
  }

  render() {
    var ul = document.createElement("ul");
    this.items.forEach((item) => {
      var li = document.createElement("li");
      var p = document.createElement("p");
      p.textContent = item;
      li.appendChild(p);
      ul.appendChild(li);
    });
    return ul;
  }
}
