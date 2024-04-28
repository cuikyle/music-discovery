import React from "react";
import {useRoutes} from "react-router-dom";
import {ThemeProvider} from "@mui/material/styles";
import {baseTheme} from './assets/Theme-variable'
import Routes from "./routes/Router";

const App = () => {
  const routing = useRoutes(Routes);
  return (
    <ThemeProvider theme={baseTheme}>
      {routing}
    </ThemeProvider>
  );
};

export default App;
