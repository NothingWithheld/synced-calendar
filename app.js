import "react-dates/initialize";
import React from "react";
import { DateRangePicker } from "react-dates";
import "react-dates/lib/css/_datepicker.css";
import ReactDOM from "react-dom";
import moment from "moment";

ReactDOM.render(
  <DateRangePicker
    startDate={moment()}
    startDateId="test1"
    endDate={moment()}
    endDateId="test2"
    onDatesChange={(_) => {}}
    focusedInput={null}
    onFocusChange={(_) => {}}
  />,
  document.getElementById("elm")
);
