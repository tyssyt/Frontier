package tys.frontier.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Stack;

public class MapStack<K,V> implements Iterable<Map<K,V>> {

    private Stack<Map<K, V>> stack = new Stack<>();

    //basic stack operations
    public Map<K,V> push(Map<K, V> map) {
        return stack.push(map);
    }

    public Map<K,V> push() {
        return stack.push(new HashMap<>());
    }

    public Map<K,V> pop() {
        return stack.pop();
    }

    public Map<K,V> peek() {
        return stack.peek();
    }

    public int stackHeight() {
        return stack.size();
    }

    public boolean isEmpty() {
        return stack.isEmpty();
    }

    public void clear() {
        stack.clear();
    }

    //operations for set elements
    public boolean contains(K t) {
        for (Map<K,V> map : this)
            if (map.containsKey(t))
                return true;
        return false;
    }

    public V put(K key, V value) {
        return stack.peek().put(key, value);
    }

    public void putAll(Map<? extends  K, ?extends V> map) {
        stack.peek().putAll(map);
    }

    public V get(K key) {
        for (Map<K,V> map : this) {
            V v = map.get(key);
            if (v != null)
                return v;
        }
        return null;
    }


    //iterators
    @Override
    public Iterator<Map<K, V>> iterator() {
        return stack.iterator();
    }


}
