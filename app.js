import "react-dates/initialize";
import React from "react";
import { DateRangePicker } from "react-dates";
import "react-dates/lib/css/_datepicker.css";
import ReactDOM from "react-dom";
import moment from "moment";

const momentToString = (date) => date.calendar();

const stringToMoment = (dateString) => moment(dateString);

class CustomDatepicker extends HTMLElement {
  constructor() {
    super();

    this._startDate = null;
    this._endDate = null;
    this._id = null;
    this.focusedInput = null;
  }

  set dates([startDate, endDate]) {
    this._startDate = stringToMoment(startDate);
    this._endDate = stringToMoment(endDate);

    this.render();
  }

  set id(id) {
    this._id = id;
  }

  render() {
    console.log(this);
    console.log(this._startDate);
    console.log(this._endDate);
    console.log(this._id);
    ReactDOM.render(
      <DateRangePicker
        startDate={this._startDate || moment()}
        startDateId={"start-id--" + this._id}
        endDate={this._endDate || moment()}
        endDateId={"end-id--" + this._id}
        onDatesChange={this.onDatesChange.bind(this)}
        focusedInput={this.focusedInput}
        onFocusChange={this.onFocusChange.bind(this)}
      />,
      this
    );
  }

  onDatesChange({ startDate, endDate }) {
    console.log({ startDate, endDate });
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
    console.log({ focusedInput });
    this.focusedInput = focusedInput;
  }

  connectedCallback() {
    this.render();
  }
}

window.customElements.define("custom-datepicker", CustomDatepicker);
