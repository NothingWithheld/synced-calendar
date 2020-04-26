import "react-dates/initialize";
import React from "react";
import { DateRangePicker } from "react-dates";
import "react-dates/lib/css/_datepicker.css";
import ReactDOM from "react-dom";
import moment from "moment";

const momentToString = (date) => {
  if (date === null) return date;

  return date.format("L");
};

const stringToMoment = (date) => {
  if (date === null) return date;
  return moment(date);
};

const areDatesDifferent = (prevDate, nextString) => {
  if (prevDate === null) return nextString !== null;
  return momentToString(prevDate) !== nextString;
};

class CustomDatepicker extends HTMLElement {
  constructor() {
    super();

    this._startDate = null;
    this._endDate = null;
    this._id = null;
    this.focusedInput = null;
  }

  set dates([startDate, endDate]) {
    if (
      !areDatesDifferent(this._startDate, startDate) &&
      !areDatesDifferent(this._endDate, endDate)
    )
      return;

    this._startDate = stringToMoment(startDate);
    this._endDate = stringToMoment(endDate);

    this.render();
  }

  set id(id) {
    this._id = id;
  }

  render() {
    ReactDOM.render(
      <DateRangePicker
        startDate={this._startDate}
        startDateId={"start-id--" + this._id}
        endDate={this._endDate}
        endDateId={"end-id--" + this._id}
        onDatesChange={this.onDatesChange.bind(this)}
        focusedInput={this.focusedInput}
        onFocusChange={this.onFocusChange.bind(this)}
      />,
      this
    );
  }

  onDatesChange({ startDate, endDate }) {
    this.dispatchEvent(
      new CustomEvent("onDateChange", {
        detail: {
          startDate: momentToString(startDate),
          endDate: momentToString(endDate),
        },
      })
    );
  }

  onFocusChange(focusedInput) {
    this.focusedInput = focusedInput;
    this.render();
  }

  connectedCallback() {
    this.render();
  }
}

window.customElements.define("custom-datepicker", CustomDatepicker);
