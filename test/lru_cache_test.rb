require "minitest/autorun"

class Entry
  attr_accessor :value, :key, :left, :right
end

class LruCache
  attr_accessor :start, :end, :hash_map
  LRU_SIZE = 4
  def initialize
    @hash_map = {}
    @start = Entry.new
    @end = Entry.new
  end

  def get(key)
    if @hash_map[key]
      entry = @hash_map[key]
      remove_node(entry)
      add_at_top(entry)
      entry.value
    else
      -1
    end
  end

  def put(key, value)
    if @hash_map[key]
      entry = @hash_map[key]
      entry.value = value
      remove_node(entry)
      add_at_top(entry)
    else
      new_entry = Entry.new
      new_entry.left = nil
      new_entry.right = nil
      new_entry.value = value
      new_entry.key = key

      if @hash_map.length > LRU_SIZE
        @hash_map.delete(@end.key)
        add_at_top(new_entry)
      else
        add_at_top(new_entry)
      end
      @hash_map[key] = new_entry
    end
  end

  def add_at_top(node)
    node.right = @start
    node.left = nil
    @start.left = node unless @start
    @start = node
    @end = @start if @end == nil
  end

  def remove_node(node)
    if !node.left
      @start = node.right
    else
      node.left.right = node.right
    end
  end
end

class LruCacheTest < Minitest::Test

  def test_put
    cache = LruCache.new
    cache.put("Jack", 171)
    assert_equal 171, cache.get("Jack")
  end
end