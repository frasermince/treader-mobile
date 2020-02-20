var React = require("react");

exports.dataStateContext = React.createContext({setLoading: console.log("DEFAULT LOADING"), setError: console.log("DEFAULT ERROR")});
