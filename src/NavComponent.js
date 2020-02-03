import React, { useState, useEffect, forwardRef, useImperativeHandle } from 'react'

import {
  StyleSheet,
  Text,
  View,
  FlatList,
  TouchableHighlight,
  Modal,
  TouchableOpacity,
  Platform,
  SafeAreaView
} from 'react-native';


import Icon from 'react-native-vector-icons/EvilIcons'


const Nav = (props) => {
  let [error, setError] = useState('')
  let [dataSource, setDataSource] = useState(props.toc)
  let show = function() {
    props.setShowNav(true);
  }

  let hide = function() {
    props.setShowNav(false);
  }

  let _onPress = function(item) {
    // var item = this.props.toc[event.selectedIndex];
    if(props.display) {
      props.display(item.href);
    }
    hide();
  }
  let renderRow = function(row) {
    return (
      <TouchableHighlight onPress={() => _onPress(row)}>
        <View style={styles.row}>
          <Text style={styles.title}>
            {row.label}
          </Text>
        </View>
      </TouchableHighlight>
    );
  }

  useEffect(function() {
    if (props.shown) {
      show();
    } else {
      hide();
    }
  }, [props.shown]);

  useEffect(function() {
    setDataSource(props.toc);
  }, [props.toc.length]);

  return (
    <View style={styles.container}>
      <Modal
        animationType={"slide"}
        visible={props.shown}
        onRequestClose={() => hide()}
        >
        <SafeAreaView style={{flex: 1}}>
          <View
            style={styles.header}>
            <Text style={styles.headerTitle}>Table of Contents</Text>
            <TouchableOpacity style={styles.backButton}
              onPress={() => hide()}>
              <Icon name="close" size={34} />
            </TouchableOpacity>
          </View>
          <FlatList
            style={styles.container}
            data={dataSource}
            renderItem={(row) => {
              return renderRow(row.item);
            }}
            keyExtractor={item => item.id}
            ItemSeparatorComponent={() => <View style={styles.separator} />}
          />
        </SafeAreaView>
      </Modal>
    </View>
  );
};


const styles = StyleSheet.create({
  navBar: {
    backgroundColor: '#f7f7f7',
    height: 40,
    flex: 1,
    flexDirection: 'row',
    borderBottomColor: "#b2b2b2",
    borderBottomWidth: .5,
  },
  toc: {
    flex: 14,
  },
  button: {
    marginTop: 8,
    marginRight: 4
  },
  buttonLabel: {
    color: "#007AFF",
    textAlign: 'center',
    fontSize: 16,
  },
  container: {
    backgroundColor: 'white',
  },
  navTitle: {
    fontSize: 16,
    textAlign: 'center',
    marginTop: 10,
    flex: 20,
    marginRight: -40,
    fontWeight: "bold",
    // fontFamily: "georgia",
  },
  row: {
    flexDirection: 'row',
    // justifyContent: 'center',
    padding: 10,
    backgroundColor: '#FFFFFF',
    overflow: "hidden",
  },
  title: {
    fontFamily: "georgia",
  },
  separator: {
    height: 1,
    backgroundColor: '#CCCCCC',
  },
  container: {
    flex: 1,
    backgroundColor: '#FFFFFF',
  },
  headerTitle: {
    textAlign: 'center',
    fontSize: 22,
    fontWeight: '400',
    color: '#000',
    position: 'absolute',
    top: 20,
    left: 0,
    right: 0,
    ...Platform.select({
      ios: {
        fontFamily: "Baskerville",
      },
      android: {
        fontFamily: "serif"
      },
    }),
  },
  header: {
    backgroundColor: "#cdcdcd",
    paddingTop: 0,
    top: 0,
    ...Platform.select({
      ios: {
        height: 64,
      },
      android: {
        height: 54,
      },
    }),
    right: 0,
    left: 0,
    borderBottomWidth: 1,
    borderBottomColor:"#000",
  },
  backButton: {
    width: 130,
    height: 30,
    position: 'absolute',
    top: 20,
    left: 20,
    padding: 0,
    flexDirection: 'row',
  },
  backButtonImage: {
    width: 30,
    height: 30,
  }
});

export default Nav;
